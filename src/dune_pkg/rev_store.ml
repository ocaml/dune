open Stdune
open Dune_vcs
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Scheduler = Dune_engine.Scheduler
module Re = Dune_re
module Flock = Dune_util.Flock
open Fiber.O

module Object = struct
  type t = Sha1 of string

  let compare (Sha1 x) (Sha1 y) = String.compare x y
  let to_string (Sha1 s) = s
  let equal (Sha1 x) (Sha1 y) = String.equal x y
  let to_dyn (Sha1 s) = Dyn.string s
  let hash (Sha1 s) = String.hash s

  type resolved = t

  let of_sha1 s =
    if String.length s = 40
       && String.for_all s ~f:(function
         | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
         | _ -> false)
    then Some (Sha1 (String.lowercase_ascii s))
    else None
  ;;
end

module Remote = struct
  type nonrec t =
    { url : string
    ; default_branch : Object.resolved option Fiber.t
    ; branches_and_tags : Object.resolved String.Map.t Fiber.t
    }

  let default_branch t = t.default_branch
end

type t =
  { dir : Path.t
  ; remotes : (string, Remote.t) Table.t
  ; (* The mutex that needs to be acquired before touching [present_objects] *)
    object_mutexes : (Object.t, Fiber.Mutex.t) Table.t
  ; present_objects : (Object.t, unit) Table.t
  }

let with_mutex t obj ~f =
  let* () = Fiber.return () in
  let mutex =
    (* ideally, we'd like to clear this table if there's nobody queued for
       mutex, but it's not safe to do without tracking the number of fiber
       awaiting. *)
    Table.find_or_add t.object_mutexes obj ~f:(fun _ -> Fiber.Mutex.create ())
  in
  Fiber.Mutex.with_lock mutex ~f
;;

let lock_path { dir; _ } =
  let parent = dir |> Path.parent_exn in
  Path.relative parent "rev-store.lock"
;;

let rec attempt_to_lock flock lock ~max_retries =
  let sleep_duration = 0.1 in
  match Flock.lock_non_block flock lock with
  | Error e -> Fiber.return @@ Error e
  | Ok `Success -> Fiber.return (Ok `Success)
  | Ok `Failure ->
    if max_retries > 0
    then
      let* () = Scheduler.sleep sleep_duration in
      attempt_to_lock flock lock ~max_retries:(max_retries - 1)
    else Fiber.return (Ok `Failure)
;;

let with_flock lock_path ~f =
  let open Fiber.O in
  let parent = Path.parent_exn lock_path in
  Path.mkdir_p parent;
  let fd =
    Unix.openfile
      (Path.to_string lock_path)
      [ Unix.O_CREAT; O_WRONLY; O_SHARE_DELETE; Unix.O_CLOEXEC ]
      0o600
  in
  let flock = Flock.create fd in
  let max_retries = 49 in
  Fiber.finalize
    ~finally:(fun () ->
      let+ () = Fiber.return () in
      Unix.close fd)
    (fun () ->
      attempt_to_lock flock Flock.Exclusive ~max_retries
      >>= function
      | Ok `Success ->
        Fiber.finalize
          (fun () ->
            Dune_util.Global_lock.write_pid fd;
            f ())
          ~finally:(fun () ->
            let+ () = Fiber.return () in
            Path.unlink_no_err lock_path;
            match Flock.unlock flock with
            | Ok () -> ()
            | Error ue ->
              Unix_error.Detailed.create ue ~syscall:"flock" ~arg:"unlock"
              |> Unix_error.Detailed.raise)
      | Ok `Failure ->
        let pid = Io.read_file lock_path in
        User_error.raise
          ~hints:
            [ Pp.textf
                "Another dune instance (pid %s) has locked the revision store. If this \
                 is happening in error, make sure to terminate that instance and re-run \
                 the command."
                pid
            ]
          [ Pp.textf "Couldn't acquire revision store lock after %d attempts" max_retries
          ]
      | Error error ->
        User_error.raise
          [ Pp.textf
              "Failed to get a lock for the revision store at %s: %s"
              (Path.to_string_maybe_quoted lock_path)
              (Unix.error_message error)
          ])
;;

let failure_mode = Process.Failure_mode.Return
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let env =
  (* to avoid Git translating its CLI *)
  Env.add Env.initial ~var:"LC_ALL" ~value:"C"
  (* to avoid prmompting for passwords *)
  |> Env.add ~var:"GIT_TERMINAL_PROMPT" ~value:"0"
;;

module Git_error = struct
  type t =
    { dir : Path.t
    ; args : string list
    ; exit_code : int
    ; output : string list
    }

  let raise_code_error { dir; args; exit_code; output } =
    let git = Lazy.force Vcs.git in
    Code_error.raise
      "git returned non-zero exit code"
      [ "exit code", Dyn.int exit_code
      ; "dir", Path.to_dyn dir
      ; "git", Path.to_dyn git
      ; "args", Dyn.list Dyn.string args
      ; "output", Dyn.list Dyn.string output
      ]
  ;;

  let result_get_or_code_error = function
    | Ok x -> x
    | Error t -> raise_code_error t
  ;;
end

let run_with_exit_code { dir; _ } ~allow_codes ~display args =
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let git = Lazy.force Vcs.git in
  let+ (), exit_code =
    Process.run ~dir ~display ~stdout_to ~stderr_to ~env failure_mode git args
  in
  if allow_codes exit_code
  then Ok exit_code
  else Error { Git_error.dir; args; exit_code; output = [] }
;;

let run t ~display args =
  run_with_exit_code t ~allow_codes:(Int.equal 0) ~display args
  >>| Result.map ~f:(ignore : int -> unit)
;;

let run_capture_lines { dir; _ } ~display args =
  let git = Lazy.force Vcs.git in
  let+ output, exit_code =
    Process.run_capture_lines ~dir ~display ~env failure_mode git args
  in
  if exit_code = 0 then Ok output else Error { Git_error.dir; args; exit_code; output }
;;

let run_capture_zero_separated_lines { dir; _ } args =
  let git = Lazy.force Vcs.git in
  let+ output, exit_code =
    Process.run_capture_zero_separated ~dir ~display:Quiet ~env failure_mode git args
  in
  if exit_code = 0 then Ok output else Error { Git_error.dir; args; exit_code; output }
;;

let cat_file { dir; _ } command =
  let git = Lazy.force Vcs.git in
  let failure_mode = Vcs.git_accept () in
  let stderr_to = make_stderr () in
  let stdout_to = make_stdout () in
  "cat-file" :: command
  |> Process.run ~dir ~display:Quiet ~stdout_to ~stderr_to ~env failure_mode git
  >>| Result.is_ok
;;

let rev_parse { dir; _ } rev =
  let git = Lazy.force Vcs.git in
  let+ line, code =
    Process.run_capture_line
      ~dir
      ~display:Quiet
      ~env
      Return
      git
      [ "rev-parse"; "--verify"; "--quiet"; sprintf "%s^{commit}" rev ]
  in
  if code = 0 then Some (Option.value_exn (Object.of_sha1 line)) else None
;;

let object_exists_no_lock { dir; _ } (Object.Sha1 sha1) =
  let git = Lazy.force Vcs.git in
  let+ (), code =
    Process.run ~dir ~display:Quiet ~env Return git [ "cat-file"; "-e"; sha1 ]
  in
  code = 0
;;

let object_exists ({ present_objects; _ } as t) obj =
  let* () = Fiber.return () in
  match Table.find present_objects obj with
  | Some () -> Fiber.return true
  | None ->
    Table.set present_objects obj ();
    let+ res = object_exists_no_lock t obj in
    (* We clear objects that aren't present, so that they can be re-queried
       after fetches *)
    if res then Table.set present_objects obj ();
    res
;;

let resolve_object t hash =
  with_mutex t hash ~f:(fun () -> object_exists t hash)
  >>| function
  | false -> None
  | true -> Some hash
;;

let mem_path repo (Object.Sha1 sha1) path =
  cat_file repo [ "-e"; sprintf "%s:%s" sha1 (Path.Local.to_string path) ]
;;

let show =
  let show { dir; _ } revs_and_paths =
    let git = Lazy.force Vcs.git in
    let failure_mode = Vcs.git_accept () in
    let command =
      "show"
      :: List.map revs_and_paths ~f:(function
        | `Object o -> o
        | `Path (Object.Sha1 r, path) -> sprintf "%s:%s" r (Path.Local.to_string path))
    in
    let stderr_to = make_stderr () in
    Process.run_capture ~dir ~display:Quiet ~stderr_to failure_mode git command
  in
  fun t revs_and_paths ->
    let cli_limit =
      (if Sys.win32 then 8191 else 2097152)
      - String.length "show"
      - 1 (* space *)
      - String.length (Path.to_string (Lazy.force Vcs.git))
      - 100 (* some extra safety *)
    in
    let rec loop acc batch cmd_len_remaining = function
      | [] -> List.rev batch :: acc
      | cmd :: cmds ->
        let cmd_len =
          1
          (* space separator *)
          +
          match cmd with
          | `Object o -> String.length o
          | `Path (Object.Sha1 r, path) ->
            String.length r + String.length (Path.Local.to_string path) + 1
        in
        let new_remaining = cmd_len_remaining - cmd_len in
        if new_remaining >= 0
        then loop acc (cmd :: batch) new_remaining cmds
        else loop (List.rev batch :: acc) [ cmd ] cli_limit cmds
    in
    loop [] [] cli_limit revs_and_paths
    |> List.rev
    |> Fiber.parallel_map ~f:(show t)
    >>| Result.List.all
    >>| Result.map ~f:(String.concat ~sep:"")
    >>| Result.to_option
;;

let load_or_create ~dir =
  let t =
    { dir
    ; remotes = Table.create (module String) 4
    ; present_objects = Table.create (module Object) 16
    ; object_mutexes = Table.create (module Object) 16
    }
  in
  let lock = lock_path t in
  let* () = Fiber.return () in
  let+ () =
    with_flock lock ~f:(fun () ->
      match Fpath.mkdir_p (Path.to_string dir) with
      | Already_exists -> Fiber.return ()
      | Created ->
        run t ~display:Quiet [ "init"; "--bare" ]
        >>| (function
         | Ok () -> ()
         | Error git_error -> Git_error.raise_code_error git_error)
      | exception Unix.Unix_error (e, x, y) ->
        User_error.raise
          [ Pp.textf "%s isn't a directory" (Path.to_string_maybe_quoted dir)
          ; Pp.textf "reason: %s" (Unix_error.Detailed.to_string_hum (e, x, y))
          ]
          ~hints:[ Pp.text "delete this file or check its permissions" ])
  in
  t
;;

module Commit = struct
  module T = struct
    type t =
      { path : Path.Local.t
      ; rev : Object.t
      }

    let compare { path; rev } t =
      let open Ordering.O in
      let= () = Path.Local.compare path t.path in
      Object.compare rev t.rev
    ;;

    let to_dyn { path; rev } =
      Dyn.record [ "path", Path.Local.to_dyn path; "rev", Object.to_dyn rev ]
    ;;
  end

  include T
  module C = Comparable.Make (T)
  module Set = C.Set
end

module File = struct
  module T = struct
    type t =
      | Redirect of
          { path : Path.Local.t
          ; to_ : t
          }
      | Direct of
          { path : Path.Local.t
          ; size : int
          ; hash : string
          }

    let compare = Poly.compare

    let to_dyn = function
      | Redirect _ -> Dyn.opaque ()
      | Direct { path; size; hash } ->
        Dyn.record
          [ "path", Path.Local.to_dyn path
          ; "size", Dyn.int size
          ; "hash", Dyn.string hash
          ]
    ;;
  end

  include T

  let path = function
    | Redirect p -> p.path
    | Direct p -> p.path
  ;;

  let rec size = function
    | Direct t -> t.size
    | Redirect t -> size t.to_
  ;;

  let rec hash = function
    | Direct t -> t.hash
    | Redirect t -> hash t.to_
  ;;

  module C = Comparable.Make (T)
  module Set = C.Set
end

module Entry = struct
  module T = struct
    type t =
      | File of File.t
      | Commit of Commit.t

    let compare a b =
      match a, b with
      | File a, File b -> File.compare a b
      | Commit a, Commit b -> Commit.compare a b
      | File _, Commit _ -> Ordering.Lt
      | Commit _, File _ -> Ordering.Gt
    ;;

    let to_dyn = function
      | File b -> Dyn.variant "File" [ File.to_dyn b ]
      | Commit c -> Dyn.variant "Commit" [ Commit.to_dyn c ]
    ;;
  end

  include T
  module C = Comparable.Make (T)
  module Set = C.Set

  let parse =
    let re =
      let space = Re.(rep1 space) in
      let perm = Re.(rep1 digit) in
      let hash = Re.(rep1 alnum) in
      let type_ = Re.(rep1 alpha) in
      let size = Re.(alt [ rep1 digit; str "-" ]) in
      let path = Re.(rep1 any) in
      [ perm
      ; space
      ; Re.group type_
      ; space
      ; Re.group hash
      ; space
      ; Re.group size
      ; space
      ; Re.group path
      ]
      |> Re.seq
      |> Re.compile
    in
    fun line ->
      Re.exec_opt re line
      |> Option.bind ~f:(fun m ->
        match Re.Group.get m 1 with
        | "blob" ->
          Some
            (File
               (Direct
                  { hash = Re.Group.get m 2
                  ; size = Int.of_string_exn @@ Re.Group.get m 3
                  ; path = Path.Local.of_string @@ Re.Group.get m 4
                  }))
        | "commit" ->
          Some
            (Commit
               { rev = Re.Group.get m 2 |> Object.of_sha1 |> Option.value_exn
               ; path = Path.Local.of_string @@ Re.Group.get m 4
               })
        | _ -> None)
  ;;
end

let fetch_allow_failure repo ~url obj =
  with_mutex repo obj ~f:(fun () ->
    object_exists repo obj
    >>= function
    | true -> Fiber.return `Fetched
    | false ->
      run_with_exit_code
        ~allow_codes:(fun x -> x = 0 || x = 128)
        repo
        ~display:!Dune_engine.Clflags.display
        [ "fetch"; "--no-write-fetch-head"; url; Object.to_string obj ]
      >>| (function
       | Ok 128 -> `Not_found
       | Ok 0 ->
         Table.set repo.present_objects obj ();
         `Fetched
       | Error git_error -> Git_error.raise_code_error git_error
       | _ -> assert false))
;;

let fetch repo ~url obj =
  fetch_allow_failure repo ~url obj
  >>| function
  | `Fetched -> ()
  | `Not_found ->
    User_error.raise [ Pp.textf "unable to fetch %S from %S" (Object.to_string obj) url ]
;;

module At_rev = struct
  type repo = t

  type t =
    { repo : repo
    ; revision : Object.t
    ; files : File.Set.t
    }

  let equal x y = Object.equal x.revision y.revision
  let rev t = t.revision

  module Config = struct
    type bindings = string * string

    type section =
      { name : string
      ; arg : string option
      ; bindings : bindings list
      }

    type t = section list

    module KV = struct
      module T = struct
        type t = string * string option

        let compare = Tuple.T2.compare String.compare (Option.compare String.compare)
        let to_dyn = Tuple.T2.to_dyn Dyn.string (Dyn.option Dyn.string)
      end

      include Comparable.Make (T)
    end

    let parse line =
      let open Option.O in
      let* key, value = String.lsplit2 ~on:'=' line in
      let+ section, key = String.lsplit2 ~on:'.' key in
      let arg, binding =
        match String.rsplit2 ~on:'.' key with
        | None -> None, key
        | Some (arg, binding) -> Some arg, binding
      in
      section, arg, binding, value
    ;;

    let config repo (Object.Sha1 rev) path : t Fiber.t =
      [ "config"; "--list"; "--blob"; sprintf "%s:%s" rev (Path.Local.to_string path) ]
      |> run_capture_lines repo ~display:Quiet
      >>| Git_error.result_get_or_code_error
      >>| List.fold_left ~init:KV.Map.empty ~f:(fun acc line ->
        match parse line with
        | None ->
          Code_error.raise "Couldn't parse git config line" [ "line", Dyn.string line ]
        | Some (section, arg, binding, value) ->
          KV.Map.update acc (section, arg) ~f:(function
            | None -> Some [ binding, value ]
            | Some xs -> Some ((binding, value) :: xs)))
      >>| KV.Map.foldi ~init:[] ~f:(fun (name, arg) bindings acc ->
        let section = { name; arg; bindings } in
        section :: acc)
    ;;
  end

  module Submodule = struct
    (* a submodule in [.gitmodules] can also have a [branch] but given we only
       need to resolve the commit object, we don't have to care about the
       tracking branch *)
    type t =
      { path : Path.Local.t
      ; source : string
      }

    let parse repo revision =
      let submodule_path = Path.Local.of_string ".gitmodules" in
      let* has_submodules = mem_path repo revision submodule_path in
      match has_submodules with
      | false -> Fiber.return []
      | true ->
        let+ cfg = Config.config repo revision submodule_path in
        List.filter_map cfg ~f:(function
          | { Config.name = "submodule"; arg = _; bindings } ->
            let find_key key (k, v) =
              match String.equal k key with
              | true -> Some v
              | false -> None
            in
            let path = List.find_map bindings ~f:(find_key "path") in
            let url = List.find_map bindings ~f:(find_key "url") in
            (match path, url with
             | Some path, Some source ->
               (* CR-rginberg: we need to handle submodule paths that try to escape
                  the repo *)
               let path = Path.Local.of_string path in
               Some { path; source }
             | _, _ ->
               (* CR-Leonidas-from-XIV: Loc.t for the .gitmodules? *)
               User_error.raise
                 ~hints:[ Pp.text "Make sure all git submodules specify path & url" ]
                 [ Pp.text "Submodule definition missing path or url" ])
          | _otherwise -> None)
    ;;
  end

  let files_and_submodules repo (Object.Sha1 rev) =
    run_capture_zero_separated_lines repo [ "ls-tree"; "-z"; "--long"; "-r"; rev ]
    >>| Git_error.result_get_or_code_error
    >>| List.fold_left
          ~init:(File.Set.empty, Commit.Set.empty)
          ~f:(fun (files, commits) line ->
            match Entry.parse line with
            | None -> files, commits
            | Some (File file) -> File.Set.add files file, commits
            | Some (Commit commit) -> files, Commit.Set.add commits commit)
  ;;

  let path_commit_map submodules =
    Commit.Set.fold
      submodules
      ~init:Path.Local.Map.empty
      ~f:(fun { Commit.path; rev } m ->
        match Path.Local.Map.add m path rev with
        | Ok m -> m
        | Error (Sha1 existing_rev) ->
          let (Sha1 found_rev) = rev in
          User_error.raise
            [ Pp.textf
                "Path %s specified multiple times as submodule pointing to different \
                 commits: %s and %s"
                (Path.Local.to_string path)
                found_rev
                existing_rev
            ])
  ;;

  let rec of_rev repo ~revision =
    let* files, submodules = files_and_submodules repo revision in
    let+ files =
      let commit_paths = path_commit_map submodules in
      let* submodules = Submodule.parse repo revision in
      (* It's not safe to do a parallel map because adding a remote
         requires getting the lock (which we're now holding) *)
      submodules
      |> Fiber.sequential_map ~f:(fun { Submodule.path; source } ->
        match Path.Local.Map.find commit_paths path with
        | None ->
          User_error.raise
            ~hints:
              [ Pp.text
                  "Make sure the submodule is initialized and committed in the source \
                   repository"
              ]
            [ Pp.textf
                "Submodule definition %s references non-existing path %s in repo"
                source
                (Path.Local.to_string path)
            ]
        | Some revision ->
          let* () = fetch repo ~url:source revision in
          let+ at_rev = of_rev repo ~revision in
          File.Set.map at_rev.files ~f:(fun file ->
            let path = Path.Local.append path (File.path file) in
            File.Redirect { path; to_ = file }))
      >>| List.cons files
      >>| File.Set.union_all
    in
    { repo; revision; files }
  ;;

  let content { repo; revision; files = _ } path = show repo [ `Path (revision, path) ]

  let directory_entries_recursive t path =
    (* TODO: there are much better ways of implementing this:
       1. using libgit or ocamlgit
       2. possibly using [$ git archive] *)
    File.Set.to_list t.files
    |> List.filter_map ~f:(fun (file : File.t) ->
      let file_path = File.path file in
      (* [directory_entries "foo"] shouldn't return "foo" as an entry, but
         "foo" is indeed a descendant of itself. So we filter it manually. *)
      if (not (Path.Local.equal file_path path))
         && Path.Local.is_descendant file_path ~of_:path
      then Some file
      else None)
    |> File.Set.of_list
  ;;

  let directory_entries_immediate t path =
    (* TODO: there are much better ways of implementing this:
       1. using libgit or ocamlgit
       2. possibly using [$ git archive] *)
    File.Set.filter t.files ~f:(fun (file : File.t) ->
      match Path.Local.parent (File.path file) with
      | None -> false
      | Some p -> Path.Local.equal p path)
  ;;

  let directory_entries t ~recursive path =
    (if recursive then directory_entries_recursive else directory_entries_immediate)
      t
      path
  ;;

  let check_out { repo = { dir; _ }; revision = Sha1 rev; files = _ } ~target =
    (* TODO iterate over submodules to output sources *)
    let git = Lazy.force Vcs.git in
    let temp_dir = Temp_dir.dir_for_target ~target ~prefix:"rev-store" ~suffix:rev in
    Fiber.finalize ~finally:(fun () ->
      let+ () = Fiber.return () in
      Temp.destroy Dir temp_dir)
    @@ fun () ->
    let archive = Path.relative temp_dir "archive.tar" in
    let stdout_to = Process.Io.file archive Process.Io.Out in
    let stderr_to = make_stderr () in
    let* () =
      let args = [ "archive"; "--format=tar"; rev ] in
      let+ (), exit_code =
        Process.run ~dir ~display:Quiet ~stdout_to ~stderr_to ~env failure_mode git args
      in
      if exit_code <> 0
      then Git_error.raise_code_error { dir; args; exit_code; output = [] }
    in
    (* We untar things into a temp dir to make sure we don't create garbage
       in the build dir until we know can produce the files *)
    let target_in_temp_dir = Path.relative temp_dir "dir" in
    Tar.extract ~archive ~target:target_in_temp_dir
    >>| function
    | Error () -> User_error.raise [ Pp.text "failed to untar archive created by git" ]
    | Ok () ->
      Path.mkdir_p (Path.parent_exn target);
      Path.rename target_in_temp_dir target
  ;;
end

let remote =
  let hash = Re.(rep1 alnum) in
  let head_mark, head = Re.mark (Re.str "HEAD") in
  let re =
    Re.(
      compile
      @@ seq
           [ bol
           ; group hash
           ; rep1 space
           ; alt
               [ head
               ; seq
                   [ str "refs/"
                   ; alt [ str "heads"; str "tags" ]
                   ; str "/"
                   ; group (rep1 any)
                   ]
               ]
           ])
  in
  fun t ~url:(url_loc, url) ->
    let f url =
      let command = [ "ls-remote"; url ] in
      let refs =
        Fiber_lazy.create (fun () ->
          let+ hits =
            run_capture_lines t ~display:!Dune_engine.Clflags.display command
            >>| function
            | Ok lines -> lines
            | Error git_error ->
              (match git_error.exit_code with
               | 128 ->
                 User_error.raise
                   ~loc:url_loc
                   ~hints:
                     [ Pp.textf
                         "Check that this Git URL in the project configuration is \
                          correct: %S"
                         url
                     ]
                   [ Pp.text "Failed to run external command:"
                   ; User_message.command (sprintf "git ls-remote %S" url)
                   ]
               | _ -> Git_error.raise_code_error git_error)
          in
          let default_branch, branches_and_tags =
            List.fold_left
              hits
              ~init:(None, [])
              ~f:(fun (default_branch, branches_and_tags) line ->
                match Re.exec_opt re line with
                | None -> default_branch, branches_and_tags
                | Some group ->
                  let hash = Re.Group.get group 1 |> Object.of_sha1 |> Option.value_exn in
                  if Re.Mark.test group head_mark
                  then Some hash, branches_and_tags
                  else (
                    let name = Re.Group.get group 2 in
                    default_branch, (name, hash) :: branches_and_tags))
          in
          default_branch, String.Map.of_list_exn branches_and_tags)
      in
      { Remote.url
      ; default_branch = Fiber_lazy.force refs >>| fst
      ; branches_and_tags = Fiber_lazy.force refs >>| snd
      }
    in
    Table.find_or_add t.remotes ~f url
;;

let fetch_resolved t (remote : Remote.t) revision =
  let* () = fetch t ~url:remote.url revision in
  At_rev.of_rev t ~revision
;;

let resolve_revision t (remote : Remote.t) ~revision =
  let* branches_and_tags = remote.branches_and_tags in
  match String.Map.find branches_and_tags revision with
  | Some obj as s ->
    let+ () = fetch t ~url:remote.url obj in
    s
  | None ->
    rev_parse t revision
    >>= (function
     | None -> Fiber.return None
     | Some obj -> resolve_object t obj)
;;

let fetch_object t (remote : Remote.t) revision =
  fetch_allow_failure t ~url:remote.url revision
  >>= function
  | `Not_found -> Fiber.return None
  | `Fetched -> At_rev.of_rev t ~revision >>| Option.some
;;

let content_of_files t files =
  match files with
  | [] -> Fiber.return []
  | _ :: _ ->
    let+ out =
      List.map files ~f:(fun file -> `Object (File.hash file))
      |> show t
      >>| function
      | Some s -> s
      | None ->
        Code_error.raise
          "content_of_files failed"
          [ "files", Dyn.(list File.to_dyn) files ]
    in
    let rec loop acc pos = function
      | [] ->
        assert (pos = String.length out);
        acc
      | (file : File.t) :: files ->
        let size = File.size file in
        let acc = String.sub out ~pos ~len:size :: acc in
        loop acc (pos + size) files
    in
    List.rev (loop [] 0 files)
;;

let get =
  Fiber_lazy.create (fun () ->
    let dir =
      Path.L.relative
        (Path.of_string (Xdg.cache_dir (Lazy.force Dune_util.xdg)))
        [ "dune"; "git-repo" ]
    in
    load_or_create ~dir)
  |> Fiber_lazy.force
;;
