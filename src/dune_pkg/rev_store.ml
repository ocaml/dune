open Stdune
open Dune_vcs
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Scheduler = Dune_engine.Scheduler
module Re = Dune_re
module Flock = Dune_util.Flock
open Fiber.O

type t = { dir : Path.t }

let lock_path { dir } =
  let parent = dir |> Path.parent_exn in
  Path.relative parent "rev-store.lock"
;;

module Rev = struct
  type t = Rev of string

  let compare (Rev a) (Rev b) = String.compare a b
  let to_dyn (Rev r) = Dyn.variant "Rev" [ Dyn.string r ]
end

let tar =
  lazy
    (match Bin.which ~path:(Env_path.path Env.initial) "tar" with
     | Some x -> x
     | None -> Dune_engine.Utils.program_not_found "tar" ~loc:None)
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
  let out = Unix.out_channel_of_descr fd in
  let flock = Flock.create fd in
  let max_retries = 49 in
  Fiber.finalize
    ~finally:(fun () ->
      let+ () = Fiber.return () in
      close_out out)
    (fun () ->
      attempt_to_lock flock Flock.Exclusive ~max_retries
      >>= function
      | Ok `Success ->
        Fiber.finalize
          (fun () ->
            Printf.fprintf out "%d\n%!" (Unix.getpid ());
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

let equal { dir } t = Path.equal dir t.dir
let failure_mode = Process.Failure_mode.Return
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

(* to avoid Git translating its CLI *)
let env = Env.add Env.initial ~var:"LC_ALL" ~value:"C"

let git_code_error ~dir ~args ~exit_code ~output =
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

let run { dir } ~display args =
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let git = Lazy.force Vcs.git in
  let+ (), exit_code =
    Process.run ~dir ~display ~stdout_to ~stderr_to ~env failure_mode git args
  in
  if exit_code <> 0 then git_code_error ~dir ~args ~exit_code ~output:[]
;;

let run_capture_line { dir } args =
  let git = Lazy.force Vcs.git in
  let+ output, exit_code =
    Process.run_capture_line ~dir ~display:Quiet ~env failure_mode git args
  in
  if exit_code = 0
  then output
  else git_code_error ~dir ~args ~exit_code ~output:[ output ]
;;

let run_capture_lines { dir } ~display args =
  let git = Lazy.force Vcs.git in
  let+ output, exit_code =
    Process.run_capture_lines ~dir ~display ~env failure_mode git args
  in
  if exit_code = 0 then output else git_code_error ~dir ~args ~exit_code ~output
;;

let run_capture_zero_separated_lines { dir } args =
  let git = Lazy.force Vcs.git in
  let+ output, exit_code =
    Process.run_capture_zero_separated ~dir ~display:Quiet ~env failure_mode git args
  in
  if exit_code = 0 then output else git_code_error ~dir ~args ~exit_code ~output
;;

let cat_file { dir } command =
  let git = Lazy.force Vcs.git in
  let failure_mode = Vcs.git_accept () in
  let stderr_to = make_stderr () in
  let stdout_to = make_stdout () in
  "cat-file" :: command
  |> Process.run ~dir ~display:Quiet ~stdout_to ~stderr_to ~env failure_mode git
  >>| Result.is_ok
;;

let mem repo ~rev = cat_file repo [ "-t"; rev ]

let mem_path repo rev path =
  let (Rev.Rev rev) = rev in
  cat_file repo [ "-e"; sprintf "%s:%s" rev (Path.Local.to_string path) ]
;;

let ref_type =
  let hash = Re.(rep1 alnum) in
  let re =
    Re.(
      compile
      @@ seq
           [ bol
           ; hash
           ; rep1 space
           ; str "refs/"
           ; group (alt [ str "heads"; str "tags" ])
           ; str "/"
           ])
  in
  fun t ~source ~ref ->
    let command = [ "ls-remote"; "--heads"; "--tags"; source; ref ] in
    let+ hits = run_capture_lines t ~display:!Dune_engine.Clflags.display command in
    List.find_map hits ~f:(fun line ->
      match Re.exec_opt re line with
      | None -> None
      | Some m ->
        (match Re.Group.get m 1 with
         | "heads" -> Some `Head
         | "tags" -> Some `Tag
         | _ -> None))
;;

let show =
  let show { dir } revs_and_paths =
    let git = Lazy.force Vcs.git in
    let failure_mode = Vcs.git_accept () in
    let command =
      "show"
      :: List.map revs_and_paths ~f:(function
        | `Object o -> o
        | `Path (Rev.Rev r, path) -> sprintf "%s:%s" r (Path.Local.to_string path))
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
          | `Path (Rev.Rev r, path) ->
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
  let t = { dir } in
  let lock = lock_path t in
  let* () = Fiber.return () in
  let+ () =
    with_flock lock ~f:(fun () ->
      match Fpath.mkdir_p (Path.to_string dir) with
      | Already_exists -> Fiber.return ()
      | Created -> run t ~display:Quiet [ "init"; "--bare" ]
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
      ; rev : Rev.t
      }

    let compare { path; rev } t =
      let open Ordering.O in
      let= () = Path.Local.compare path t.path in
      Rev.compare rev t.rev
    ;;

    let to_dyn { path; rev } =
      Dyn.record [ "path", Path.Local.to_dyn path; "rev", Rev.to_dyn rev ]
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
               { rev = Rev (Re.Group.get m 2)
               ; path = Path.Local.of_string @@ Re.Group.get m 4
               })
        | _ -> None)
  ;;
end

module At_rev = struct
  type repo = t

  type t =
    { repo : repo
    ; revision : Rev.t
    ; source : string
    ; files : File.Set.t
    }

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

    let config repo revision path : t Fiber.t =
      let (Rev.Rev rev) = revision in
      [ "config"; "--list"; "--blob"; sprintf "%s:%s" rev (Path.Local.to_string path) ]
      |> run_capture_lines repo ~display:Quiet
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

  let files_and_submodules repo (Rev.Rev rev) =
    run_capture_zero_separated_lines repo [ "ls-tree"; "-z"; "--long"; "-r"; rev ]
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
        | Error (Rev existing_rev) ->
          let (Rev found_rev) = rev in
          User_error.raise
            [ Pp.textf
                "Path %s specified multiple times as submodule pointing to different \
                 commits: %s and %s"
                (Path.Local.to_string path)
                found_rev
                existing_rev
            ])
  ;;

  let rec of_rev repo ~add_remote ~revision ~source =
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
          let* () = add_remote source in
          let+ at_rev = of_rev repo ~add_remote ~revision ~source in
          File.Set.map at_rev.files ~f:(fun file ->
            let path = Path.Local.append path (File.path file) in
            File.Redirect { path; to_ = file }))
      >>| List.cons files
      >>| File.Set.union_all
    in
    { repo; revision; source; files }
  ;;

  let content { repo; revision; source = _; files = _ } path =
    show repo [ `Path (revision, path) ]
  ;;

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

  let repo_equal = equal

  let equal { repo; revision = Rev revision; source; files } t =
    let (Rev revision') = t.revision in
    repo_equal repo t.repo
    && String.equal revision revision'
    && String.equal source t.source
    && File.Set.equal files t.files
  ;;

  let opam_url { revision = Rev rev; source; repo = _; files = _ } =
    OpamUrl.parse (sprintf "%s#%s" source rev)
  ;;

  let check_out { repo = { dir }; revision = Rev rev; source = _; files = _ } ~target =
    (* TODO iterate over submodules to output sources *)
    let git = Lazy.force Vcs.git in
    let tar = Lazy.force tar in
    let temp_dir = Temp.create Dir ~prefix:"rev-store" ~suffix:rev in
    let archive_file = Path.relative temp_dir "archive.tar" in
    let stdout_to = Process.Io.file archive_file Process.Io.Out in
    let stderr_to = make_stderr () in
    let* () =
      let args = [ "archive"; "--format=tar"; rev ] in
      let+ (), exit_code =
        Process.run ~dir ~display:Quiet ~stdout_to ~stderr_to ~env failure_mode git args
      in
      if exit_code <> 0 then git_code_error ~dir ~args ~exit_code ~output:[]
    in
    let stdout_to = make_stdout () in
    let stderr_to = make_stderr () in
    let+ () =
      let args = [ "xf"; Path.to_string archive_file ] in
      let+ (), exit_code =
        Process.run ~dir:target ~display:Quiet ~stdout_to ~stderr_to failure_mode tar args
      in
      if exit_code <> 0
      then
        Code_error.raise
          "tar returned non-zero exit code"
          [ "exit code", Dyn.int exit_code
          ; "dir", Path.to_dyn target
          ; "tar", Path.to_dyn tar
          ; "args", Dyn.list Dyn.string args
          ]
    in
    ()
  ;;
end

module Remote = struct
  type nonrec t =
    { repo : t
    ; handle : string
    ; source : string
    ; default_branch : string
    ; add_remote : string -> unit Fiber.t
    }

  type uninit = t

  let update ({ repo; handle; source = _; default_branch = _; add_remote = _ } as t) =
    let+ () = run repo ~display:!Dune_engine.Clflags.display [ "fetch"; handle ] in
    t
  ;;

  let don't_update t = t

  let default_branch { repo = _; handle = _; source = _; default_branch; add_remote = _ } =
    default_branch
  ;;

  let equal { repo; handle; source; default_branch; add_remote = _ } t =
    equal repo t.repo
    && String.equal handle t.handle
    && String.equal source t.source
    && String.equal default_branch t.default_branch
  ;;

  let rev_of_name { repo; handle; source; default_branch = _; add_remote } ~name =
    (* TODO handle non-existing name *)
    let* rev = run_capture_line repo [ "rev-parse"; sprintf "%s/%s" handle name ] in
    let revision = Rev.Rev rev in
    At_rev.of_rev repo ~add_remote ~revision ~source >>| Option.some
  ;;

  let rev_of_ref { repo; handle = _; source; default_branch = _; add_remote } ~ref =
    let revision = Rev.Rev ref in
    At_rev.of_rev repo ~add_remote ~revision ~source >>| Option.some
  ;;
end

let remote_exists dir ~name =
  let stderr_to = make_stderr () in
  let command = [ "remote"; "--verbose" ] in
  let+ lines =
    let git = Lazy.force Vcs.git in
    Process.run_capture_lines ~dir ~display:Quiet ~stderr_to ~env Strict git command
  in
  lines
  |> List.find ~f:(fun line ->
    match String.lsplit2 ~on:'\t' line with
    | None -> false
    | Some (candidate, _) -> String.equal candidate name)
  |> Option.is_some
;;

let query_head_branch =
  let re =
    Re.(
      compile
      @@ seq
           [ bol
           ; str "ref: refs/heads/"
           ; group (rep1 (diff any space))
           ; rep1 space
           ; str "HEAD"
           ; eol
           ])
  in
  fun t source ->
    let+ lines =
      run_capture_lines
        t
        ~display:!Dune_engine.Clflags.display
        [ "ls-remote"; "--symref"; source ]
    in
    List.find_map lines ~f:(fun line ->
      match Re.exec_opt re line with
      | None -> None
      | Some m -> Some (Re.Group.get m 1))
;;

let branch_of_refspec refspec =
  refspec
  |> String.drop_prefix_if_exists ~prefix:"+"
  |> String.lsplit2 ~on:':'
  |> Option.bind ~f:(fun (remote_ref, _local_ref) ->
    String.drop_prefix remote_ref ~prefix:"refs/heads/")
;;

let find_section =
  let re =
    Re.seq
      [ Re.(rep space)
      ; Re.str "[remote "
      ; Re.char '"'
      ; Re.group (Re.rep (Re.diff Re.any (Re.char '"')))
      ; Re.char '"'
      ; Re.char ']'
      ; Re.(rep space)
      ]
    |> Re.compile
  in
  fun contents ~name ->
    let rec loop (xs : Re.split_token list) =
      match xs with
      | [] -> None
      | `Text _ :: rest -> loop rest
      | `Delim delim :: rest ->
        if Re.Group.get delim 1 = name
        then
          Some
            (match rest with
             | `Text s :: _ -> s
             | _ -> "")
        else loop rest
    in
    loop (Re.split_full re contents)
;;

let read_head_branch =
  let fetch_line = Re.(compile @@ seq [ str "fetch = "; group (rep1 any); eol ]) in
  fun t handle ->
    Path.relative t.dir "config"
    |> Io.read_file ~binary:true
    |> find_section ~name:handle
    |> Option.bind ~f:(fun section ->
      String.split_lines section
      |> List.find_map ~f:(fun line ->
        Re.exec_opt fetch_line line
        |> Option.bind ~f:(fun m ->
          let refspec = Re.Group.get m 1 in
          branch_of_refspec refspec)))
;;

let remote_add t ~branch ~handle ~source =
  let* () = run ~display:Quiet t [ "remote"; "add"; "--track"; branch; handle; source ] in
  (* add a refspec to fetch the remotes' tags into <handle>/<tag> namespace *)
  run
    t
    ~display:Quiet
    [ "config"
    ; "--add"
    ; sprintf "remote.%s.fetch" handle
    ; sprintf "+refs/tags/*:refs/tags/%s/*" handle
    ]
;;

let remote_name ~source ~branch =
  let decoded_remote =
    match branch with
    | None -> source
    | Some branch -> sprintf "%s %s" source branch
  in
  decoded_remote |> Dune_digest.string |> Dune_digest.to_string
;;

let rec add_repo ({ dir } as t) ~source ~branch =
  let handle = remote_name ~source ~branch in
  let lock = lock_path t in
  with_flock lock ~f:(fun () ->
    let* exists = remote_exists dir ~name:handle in
    let+ default_branch =
      match exists, branch with
      | true, Some branch -> Fiber.return branch
      | true, None ->
        (match read_head_branch t handle with
         | Some head_branch -> Fiber.return head_branch
         | None ->
           (* the rev store is in some sort of unexpected state *)
           Code_error.raise
             (sprintf "Could not load default branch of repository")
             [ "source", Dyn.string source; "handle", Dyn.string handle ])
      | false, Some branch ->
        let+ () = remote_add t ~branch ~handle ~source in
        branch
      | false, None ->
        let* branch =
          query_head_branch t source
          >>| function
          | Some head_branch -> head_branch
          | None ->
            User_error.raise
              ~hints:
                [ Pp.textf
                    "Make sure '%s' is a valid git repository and has a HEAD branch"
                    source
                ]
              [ Pp.textf "Could not determine default branch of repository at '%s'" source
              ]
        in
        let+ () = remote_add t ~branch ~handle ~source in
        branch
    in
    let add_remote source =
      let* remote = add_repo t ~branch:None ~source in
      let+ (_ : Remote.t) = Remote.update remote in
      ()
    in
    { Remote.repo = t; handle; source; default_branch; add_remote })
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
