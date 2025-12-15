open Stdune
open Dune_vcs
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Scheduler = Dune_engine.Scheduler
module Console = Dune_console
open Fiber.O

module Object : sig
  type t

  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t
  val hash : t -> int
  val to_hex : t -> string
  val of_sha1_unsafe : string -> t

  type resolved = t

  val of_sha1 : string -> resolved option
end = struct
  type t = Sha1 of string

  let compare (Sha1 x) (Sha1 y) = String.compare x y
  let to_hex (Sha1 s) = s
  let equal (Sha1 x) (Sha1 y) = String.equal x y
  let to_dyn (Sha1 s) = Dyn.variant "Sha1" [ Dyn.opaque s ]
  let hash (Sha1 s) = String.hash s
  let of_sha1_unsafe s = Sha1 s

  type resolved = t

  let of_sha1 s =
    if
      String.length s = 40
      && String.for_all s ~f:(function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false)
    then Some (Sha1 (String.lowercase_ascii s))
    else None
  ;;
end

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
          ; hash : Object.t
          }

    let rec compare x y =
      let open Ordering.O in
      match x, y with
      | Redirect { path; to_ }, Redirect t ->
        let= () = Path.Local.compare path t.path in
        compare to_ t.to_
      | Redirect _, _ -> Lt
      | _, Redirect _ -> Gt
      | Direct { path; size; hash }, Direct t ->
        let= () = Path.Local.compare path t.path in
        let= () = Int.compare size t.size in
        Object.compare hash t.hash
    ;;

    let rec to_dyn = function
      | Redirect { path; to_ } ->
        Dyn.variant
          "Redirect"
          [ Dyn.record [ "path", Path.Local.to_dyn path; "to_", to_dyn to_ ] ]
      | Direct { path; size; hash } ->
        Dyn.variant
          "Direct"
          [ Dyn.record
              [ "path", Path.Local.to_dyn path
              ; "size", Dyn.int size
              ; "hash", Object.to_dyn hash
              ]
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

module Cache = struct
  (* CR-someday Alizter: Various LMDB  operations are able to raise [Map_full]
     when the database is full. We should handle this in a sensible way. For
     now, we allow it to raise. It won't be visible to most users due to the
     cache size we have chosen. *)

  let rev_store_cache =
    (* CR-soon Alizter: For now the cache is disabled by default. Once we add
       versioning to the cache it will be safe to enable by default. 

       When we do this we should check [Int.equal Sys.word_size 64] since
       32-bit platforms won't handle our cache size well. *)
    Dune_config.Config.make_toggle ~name:"rev_store_cache" ~default:`Disabled
  ;;

  let revision_store_dir =
    lazy
      (let path = Path.relative (Lazy.force Dune_util.cache_root_dir) "rev_store" in
       let rev_store_cache = Dune_config.Config.get rev_store_cache in
       Log.info
         [ Pp.textf
             "Revision store cache: %s"
             (Dune_config.Config.Toggle.to_string rev_store_cache)
         ];
       match rev_store_cache with
       | `Enabled ->
         Path.mkdir_p path;
         Log.info [ Pp.textf "Revision store cache location: %s" (Path.to_string path) ];
         Some path
       | `Disabled -> None)
  ;;

  let db =
    lazy
      (Lazy.force revision_store_dir
       |> Option.map ~f:(fun path ->
         Lmdb.Env.create
           ~map_size:(Int64.to_int 5_000_000_000L) (* 5 GB *)
           ~max_maps:2
           ~flags:Lmdb.Env.Flags.(no_meta_sync)
           Rw
           (Path.to_string path)))
  ;;

  let () =
    at_exit (fun () ->
      if Lazy.is_val db
      then (
        match Lazy.force db with
        | Some db -> Lmdb.Env.close db
        | None -> ()))
  ;;

  module Key = struct
    module T = struct
      type t = Object.t

      let compare = Object.compare
      let to_dyn = Object.to_dyn
    end

    include T
    module C = Comparable.Make (T)
    module Map = C.Map
    module Set = C.Set

    let conv =
      Lmdb.Conv.make
        ~serialise:(fun alloc obj ->
          Object.to_hex obj |> Lmdb.Conv.(serialise string alloc))
        ~deserialise:(fun bs ->
          Lmdb.Conv.(deserialise string bs) |> Object.of_sha1_unsafe)
        ()
    ;;
  end

  let map =
    lazy
      (Lazy.force db
       |> Option.map ~f:(fun env ->
         Lmdb.Map.create Nodup ~key:Key.conv ~value:Lmdb.Conv.string ~name:"objects" env)
      )
  ;;

  module Files_and_submodules = struct
    module Key = struct
      module T = struct
        type t = Object.t

        let compare = Object.compare
        let to_dyn = Object.to_dyn
      end

      include T
      module C = Comparable.Make (T)

      let conv =
        Lmdb.Conv.make
          ~serialise:(fun alloc obj ->
            Object.to_hex obj |> Lmdb.Conv.(serialise string alloc))
          ~deserialise:(fun bs ->
            Lmdb.Conv.(deserialise string bs) |> Object.of_sha1_unsafe)
          ()
      ;;
    end

    module Value = struct
      let conv : (File.Set.t * Commit.Set.t) Lmdb.Conv.t =
        Lmdb.Conv.make
          ~serialise:(fun alloc v ->
            Marshal.to_string v [] |> Lmdb.Conv.(serialise string alloc))
          ~deserialise:(fun bs -> Marshal.from_string Lmdb.Conv.(deserialise string bs) 0)
          ()
      ;;
    end

    let map =
      lazy
        (Lazy.force db
         |> Option.map ~f:(fun env ->
           Lmdb.Map.create Nodup ~key:Key.conv ~value:Value.conv ~name:"ls-tree" env))
    ;;

    let get key =
      let open Option.O in
      let* m = Lazy.force map in
      match Lmdb.Map.get m key with
      | exception Not_found -> None
      | v -> Some v
    ;;

    let set key value =
      ignore
      @@
      let open Option.O in
      let+ map = Lazy.force map in
      Lmdb.Map.set map key value
    ;;
  end

  let get keys =
    match Lazy.force map with
    | None -> Key.Map.empty
    | Some m ->
      Key.Set.fold keys ~init:Key.Map.empty ~f:(fun key acc ->
        match Lmdb.Map.get m key with
        | exception Not_found -> acc
        | v -> Key.Map.add_exn acc key v)
  ;;

  let set keys =
    ignore
    @@
    let open Option.O in
    let* map = Lazy.force map in
    let* env = Lazy.force db in
    Lmdb.Txn.go Rw env (fun txn ->
      Key.Map.iteri keys ~f:(fun key value -> Lmdb.Map.set ~txn map key value))
  ;;
end

module Remote = struct
  type t =
    { url : string
    ; default_branch : Object.resolved option Fiber.t
    ; refs : Object.resolved String.Map.t Fiber.t
    }

  let to_dyn { url; default_branch; refs } =
    Dyn.record
      [ "url", Dyn.string url
      ; "default_branch", Dyn.opaque default_branch
      ; "refs", Dyn.opaque refs
      ]
  ;;

  let default_branch t = t.default_branch
end

type t =
  { dir : Path.t
  ; remotes : (string, Remote.t) Table.t
  ; (* The mutex that needs to be acquired before touching [present_objects] *)
    object_mutexes : (Object.t, Fiber.Mutex.t) Table.t
  ; present_objects : (Object.t, unit) Table.t
  }

let to_dyn { dir; remotes; object_mutexes; present_objects } =
  Dyn.record
    [ (* This is an external path, so we relativize to sanitize. We don't use
         [Path.to_dyn] since it wouldn't be correct and therefore confusing. *)
      "dir", Path.Expert.try_localize_external dir |> Path.to_string |> Dyn.string
    ; "remotes", Table.to_list remotes |> Dyn.list (Dyn.pair Dyn.string Remote.to_dyn)
    ; "object_mutexes", Dyn.opaque object_mutexes
    ; ( "present_objects"
      , Table.to_list present_objects |> Dyn.list (Dyn.pair Object.to_dyn Dyn.unit) )
    ]
;;

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
  let sleep_duration = Time.Span.of_secs 0.1 in
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
  let git = Lazy.force Vcs.git in
  let+ stderr, exit_code =
    Fiber_util.Temp.with_temp_file
      ~prefix:"dune"
      ~suffix:"run_with_exit_code"
      ~dir:(Path.of_string (Filename.get_temp_dir_name ()))
      ~f:(function
        | Error exn -> raise exn
        | Ok path ->
          let+ (), exit_code =
            let stderr_to = Process.Io.file path Out in
            Process.run ~dir ~display ~stdout_to ~stderr_to ~env failure_mode git args
          in
          Io.read_file path, exit_code)
  in
  if allow_codes exit_code
  then Ok exit_code
  else (
    match exit_code with
    | 129
      when String.is_prefix ~prefix:"error: unknown option `no-write-fetch-head'" stderr
      ->
      User_error.raise
        [ User_message.command
            "Your git version doesn't support the '--no-write-fetch-head' flag. The \
             minimum supported version is Git 2.29."
        ]
        ~hints:[ User_message.command "Please update your git version." ]
    | _ -> Error { Git_error.dir; args; exit_code; output = [ stderr ] })
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

let object_exists_no_lock { dir; _ } obj =
  let git = Lazy.force Vcs.git in
  let+ (), code =
    Process.run
      ~dir
      ~display:Quiet
      ~env
      Return
      git
      [ "cat-file"; "-e"; Object.to_hex obj ]
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

let mem_path repo obj path =
  cat_file repo [ "-e"; sprintf "%s:%s" (Object.to_hex obj) (Path.Local.to_string path) ]
;;

let show =
  let show { dir; _ } revs_and_paths =
    let git = Lazy.force Vcs.git in
    let failure_mode = Vcs.git_accept () in
    let command =
      "show"
      :: List.map revs_and_paths ~f:(function
        | `Object o -> Object.to_hex o
        | `Path (r, path) -> sprintf "%s:%s" (Object.to_hex r) (Path.Local.to_string path))
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
          | `Object o -> String.length (Object.to_hex o)
          | `Path (r, path) ->
            String.length (Object.to_hex r)
            + String.length (Path.Local.to_string path)
            + 1
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
                  { hash = Re.Group.get m 2 |> Object.of_sha1 |> Option.value_exn
                  ; size = Re.Group.get m 3 |> Int.of_string_exn
                  ; path = Re.Group.get m 4 |> Path.Local.of_string
                  }))
        | "commit" ->
          Some
            (Commit
               { rev = Re.Group.get m 2 |> Object.of_sha1 |> Option.value_exn
               ; path = Re.Group.get m 4 |> Path.Local.of_string
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
        ~allow_codes:(Int.equal 0)
        repo
        ~display:!Dune_engine.Clflags.display
        [ "fetch"; "--no-write-fetch-head"; url; Object.to_hex obj ]
      >>| (function
       | Ok 0 ->
         Table.set repo.present_objects obj ();
         `Fetched
       | Error { Git_error.exit_code; output; _ } when exit_code = 128 ->
         `Not_found output
       | Error git_error -> Git_error.raise_code_error git_error
       | _ -> assert false))
;;

let fetch repo ~url obj =
  fetch_allow_failure repo ~url obj
  >>| function
  | `Fetched -> ()
  | `Not_found output ->
    User_error.raise
      ([ Pp.textf
           "Dune was unable to fetch %S from %S due to the following git fetch error:"
           (Object.to_hex obj)
           url
       ]
       @ List.map ~f:Pp.verbatim output)
;;

module Debug = struct
  let files_and_submodules_cache = ref false
  let content_of_files_cache = ref false

  type t =
    { name : string
    ; payload : (string * Dyn.t) list
    }

  let to_dyn { name; payload } =
    Dyn.Tuple [ Dyn.string name; Dyn.list (Dyn.pair Dyn.string Fun.id) payload ]
  ;;

  let print : string -> (string * Dyn.t) list -> unit =
    fun name payload -> Console.print [ to_dyn { name; payload } |> Dyn.pp ]
  ;;
end

module At_rev = struct
  type repo = t

  type t =
    { repo : repo
    ; revision : Object.t
    ; files : File.Set.t
    ; recursive_directory_entries : File.Set.t Path.Local.Table.t
    ; submodules : Object.t Path.Local.Map.t
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

    let config repo rev path : t Fiber.t =
      [ "config"
      ; "--list"
      ; "--blob"
      ; sprintf "%s:%s" (Object.to_hex rev) (Path.Local.to_string path)
      ]
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

  let files_and_submodules repo key =
    let cached = Cache.Files_and_submodules.get key in
    if !Debug.files_and_submodules_cache
    then Debug.print "files_and_submodules" [ "cached", Dyn.option Dyn.opaque cached ];
    match cached with
    | Some v -> Fiber.return v
    | None ->
      let+ value =
        run_capture_zero_separated_lines
          repo
          [ "ls-tree"; "-z"; "--long"; "-r"; Object.to_hex key ]
        >>| Git_error.result_get_or_code_error
        >>| List.fold_left
              ~init:(File.Set.empty, Commit.Set.empty)
              ~f:(fun (files, commits) line ->
                match Entry.parse line with
                | None -> files, commits
                | Some (File file) -> File.Set.add files file, commits
                | Some (Commit commit) -> files, Commit.Set.add commits commit)
      in
      Cache.Files_and_submodules.set key value;
      value
  ;;

  let path_commit_map submodules =
    Commit.Set.fold
      submodules
      ~init:Path.Local.Map.empty
      ~f:(fun { Commit.path; rev } m ->
        match Path.Local.Map.add m path rev with
        | Ok m -> m
        | Error existing_rev ->
          User_error.raise
            [ Pp.textf
                "Path %s specified multiple times as submodule pointing to different \
                 commits: %s and %s"
                (Path.Local.to_string path)
                (Object.to_hex rev)
                (Object.to_hex existing_rev)
            ])
  ;;

  let rec of_rev repo ~revision =
    let* files, submodules = files_and_submodules repo revision in
    let commit_paths = path_commit_map submodules in
    let+ files =
      let* submodules = Submodule.parse repo revision in
      (* It's not safe to do a parallel map because adding a remote
         requires getting the lock (which we're now holding) *)
      Fiber.sequential_map submodules ~f:(fun { Submodule.path; source } ->
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
    let recursive_directory_entries =
      let recursive_directory_entries =
        Path.Local.Table.create (File.Set.cardinal files)
      in
      (* Build a table mapping each directory path to the set of files under it
         in the directory hierarchy. *)
      File.Set.iter files ~f:(fun file ->
        (* Add [file] to the set of files under each directory which is an
           ancestor of [file]. *)
        let rec loop = function
          | None -> ()
          | Some parent ->
            let recursive_directory_entries_of_parent =
              Path.Local.Table.find_or_add
                recursive_directory_entries
                parent
                ~f:(Fun.const File.Set.empty)
            in
            let recursive_directory_entries_of_parent =
              File.Set.add recursive_directory_entries_of_parent file
            in
            Path.Local.Table.set
              recursive_directory_entries
              parent
              recursive_directory_entries_of_parent;
            loop (Path.Local.parent parent)
        in
        loop (File.path file |> Path.Local.parent));
      recursive_directory_entries
    in
    { repo; revision; files; recursive_directory_entries; submodules = commit_paths }
  ;;

  let content
        { repo; revision; files = _; recursive_directory_entries = _; submodules = _ }
        path
    =
    show repo [ `Path (revision, path) ]
  ;;

  let directory_entries_recursive t path =
    Path.Local.Table.find t.recursive_directory_entries path
    |> Option.value ~default:File.Set.empty
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

  let check_out
        { repo = { dir; _ }
        ; revision
        ; files = _
        ; recursive_directory_entries = _
        ; submodules
        }
        ~target
    =
    let git = Lazy.force Vcs.git in
    let temp_dir =
      Temp_dir.dir_for_target ~target ~prefix:"rev-store" ~suffix:(Object.to_hex revision)
    in
    Fiber.finalize ~finally:(fun () ->
      let+ () = Fiber.return () in
      Temp.destroy Dir temp_dir)
    @@ fun () ->
    let stderr_to = make_stderr () in
    let* archives =
      let all = Path.Local.Map.add_exn submodules Path.Local.root revision in
      Path.Local.Map.to_list all
      |> Fiber.parallel_map ~f:(fun (path, rev) ->
        let archive = Path.relative temp_dir (sprintf "%s.tar" (Object.to_hex rev)) in
        let stdout_to = Process.Io.file archive Process.Io.Out in
        let args = [ "archive"; "--format=tar"; Object.to_hex rev ] in
        let+ (), exit_code =
          Process.run ~dir ~display:Quiet ~stdout_to ~stderr_to ~env failure_mode git args
        in
        if exit_code <> 0
        then Git_error.raise_code_error { dir; args; exit_code; output = [] };
        path, archive)
    in
    (* We untar things into a temp dir to make sure we don't create garbage
       in the build dir until we know can produce the files *)
    let target_in_temp_dir = Path.relative temp_dir "dir" in
    let+ () =
      (* We don't necessarily need to unpack things sequentially, but it's the
         easiest thing to do *)
      Fiber.sequential_iter archives ~f:(fun (path, archive) ->
        let target_in_temp_dir = Path.append_local target_in_temp_dir path in
        Archive_driver.extract Archive_driver.tar ~archive ~target:target_in_temp_dir
        >>| function
        | Error () ->
          User_error.raise [ Pp.text "failed to untar archive created by git" ]
        | Ok () -> ())
    in
    Path.rename target_in_temp_dir target
  ;;
end

let remote =
  let hash = Re.(rep1 alnum) in
  let head_mark, head = Re.mark (Re.str "HEAD") in
  let ref = Re.(group (seq [ str "refs/"; rep1 any ])) in
  let re = Re.(compile @@ seq [ bol; group hash; rep1 space; alt [ head; ref ] ]) in
  fun t ~loc:url_loc ~url ->
    let f url =
      let command = [ "ls-remote"; url ] in
      let refs =
        Fiber.Lazy.create (fun () ->
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
          let default_branch, refs =
            List.fold_left hits ~init:(None, []) ~f:(fun (default_branch, refs) line ->
              match Re.exec_opt re line with
              | None -> default_branch, refs
              | Some group ->
                let hash = Re.Group.get group 1 |> Object.of_sha1 |> Option.value_exn in
                if Re.Mark.test group head_mark
                then Some hash, refs
                else (
                  let name = Re.Group.get group 2 in
                  let entry = name, hash in
                  default_branch, entry :: refs))
          in
          default_branch, String.Map.of_list_exn refs)
      in
      { Remote.url
      ; default_branch = Fiber.Lazy.force refs >>| fst
      ; refs = Fiber.Lazy.force refs >>| snd
      }
    in
    Table.find_or_add t.remotes ~f url
;;

let fetch_resolved t (remote : Remote.t) revision =
  let* () = fetch t ~url:remote.url revision in
  At_rev.of_rev t ~revision
;;

let resolve_revision t (remote : Remote.t) ~revision =
  let* refs = remote.refs in
  let obj =
    match String.Map.find refs revision with
    | Some _ as obj -> obj
    | None ->
      (* revision was not found as-is, try formatting as branch/tag *)
      let lookup_in format = String.Map.find refs (sprintf format revision) in
      let as_branch = lookup_in "refs/heads/%s" in
      let as_tag = lookup_in "refs/tags/%s" in
      (match as_branch, as_tag with
       | (Some _ as obj), None -> obj
       | None, (Some _ as obj) -> obj
       | None, None -> None
       | Some branch_obj, Some tag_obj ->
         (match Object.equal branch_obj tag_obj with
          | true -> Some branch_obj
          | false ->
            let hints =
              [ Pp.textf "If you want to specify a tag use refs/tags/%s" revision
              ; Pp.textf "If you want to specify a branch use refs/branches/%s" revision
              ]
            in
            User_error.raise
              ~hints
              [ Pp.textf "Reference %S in remote %S is ambiguous" revision remote.url ]))
  in
  match obj with
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
  | `Not_found git_output -> Fiber.return (Error git_output)
  | `Fetched -> At_rev.of_rev t ~revision >>| Result.ok
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

let content_of_files t files =
  let keys = List.map files ~f:(fun file -> File.hash file, file) in
  let cached = Cache.get (Cache.Key.Set.of_list_map keys ~f:fst) in
  let uncached =
    List.filter_map keys ~f:(fun (key, file) ->
      if Cache.Key.Map.mem cached key then None else Some (key, file))
  in
  if !Debug.content_of_files_cache
  then
    Debug.print
      "contents_of_files"
      [ "files", Dyn.list File.to_dyn files
      ; "cached", Cache.Key.Map.to_dyn Dyn.string cached
      ];
  content_of_files t (List.map ~f:snd uncached)
  >>| function
  | [] -> List.map keys ~f:(fun (key, _) -> Cache.Key.Map.find_exn cached key)
  | to_write ->
    let to_write =
      List.combine (List.map ~f:fst uncached) to_write
      |> Cache.Key.Map.of_list_reduce ~f:(fun x _y -> x)
    in
    Cache.set to_write;
    List.map keys ~f:(fun (key, _) ->
      match Cache.Key.Map.find cached key with
      | Some s -> s
      | None -> Cache.Key.Map.find_exn to_write key)
;;

let git_repo_dir =
  lazy
    (let dir = Path.relative (Lazy.force Dune_util.cache_root_dir) "git-repo" in
     Log.info [ Pp.textf "Git repository cache location: %s" (Path.to_string dir) ];
     dir)
;;

let get =
  Fiber.Lazy.create (fun () -> load_or_create ~dir:(Lazy.force git_repo_dir))
  |> Fiber.Lazy.force
;;
