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

type rev = Rev of string

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
let display = Display.Quiet
let failure_mode = Process.Failure_mode.Strict
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let run { dir } =
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let git = Lazy.force Vcs.git in
  Process.run ~dir ~display ~stdout_to ~stderr_to failure_mode git
;;

let run_capture_line { dir } =
  let git = Lazy.force Vcs.git in
  Process.run_capture_line ~dir ~display failure_mode git
;;

let run_capture_lines { dir } =
  let git = Lazy.force Vcs.git in
  Process.run_capture_lines ~dir ~display failure_mode git
;;

let run_capture_zero_separated_lines { dir } =
  let git = Lazy.force Vcs.git in
  Process.run_capture_zero_separated ~dir ~display failure_mode git
;;

let show =
  let show { dir } revs_and_paths =
    let git = Lazy.force Vcs.git in
    let failure_mode = Vcs.git_accept () in
    let command =
      "show"
      :: List.map revs_and_paths ~f:(function
        | `Object o -> o
        | `Path (Rev r, path) -> sprintf "%s:%s" r (Path.Local.to_string path))
    in
    let stderr_to = make_stderr () in
    Process.run_capture ~dir ~display ~stderr_to failure_mode git command
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
          | `Path (Rev r, path) ->
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
      | Created -> run t [ "init"; "--bare" ]
      | exception Unix.Unix_error (e, x, y) ->
        User_error.raise
          [ Pp.textf "%s isn't a directory" (Path.to_string_maybe_quoted dir)
          ; Pp.textf "reason: %s" (Unix_error.Detailed.to_string_hum (e, x, y))
          ]
          ~hints:[ Pp.text "delete this file or check its permissions" ])
  in
  t
;;

module File = struct
  module T = struct
    type t =
      { path : Path.Local.t
      ; size : int
      ; hash : string
      }

    let compare { path; size; hash } t =
      let open Ordering.O in
      let= () = Path.Local.compare path t.path in
      let= () = Int.compare size t.size in
      String.compare hash t.hash
    ;;

    let to_dyn { hash; path; size } =
      Dyn.record
        [ "path", Path.Local.to_dyn path; "size", Dyn.int size; "hash", Dyn.string hash ]
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
      let size = Re.(rep1 digit) in
      let path = Re.(rep1 any) in
      [ perm
      ; space
      ; type_
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
      let m = Re.exec re line in
      { hash = Re.Group.get m 1
      ; size = Int.of_string_exn @@ Re.Group.get m 2
      ; path = Path.Local.of_string @@ Re.Group.get m 3
      }
  ;;

  let path t = t.path
end

module At_rev = struct
  type nonrec t =
    { repo : t
    ; revision : rev
    ; files_at_rev : File.Set.t
    }

  let content { repo; revision; files_at_rev = _ } path =
    show repo [ `Path (revision, path) ]
  ;;

  let directory_entries { repo = _; files_at_rev; revision = _ } path =
    (* TODO: there are much better ways of implementing this:
       1. using libgit or ocamlgit
       2. possibly using [$ git archive] *)
    File.Set.filter files_at_rev ~f:(fun (file : File.t) ->
      Path.Local.is_descendant file.path ~of_:path)
  ;;

  let equal { repo; revision = Rev revision; files_at_rev } t =
    let (Rev revision') = t.revision in
    equal repo t.repo
    && String.equal revision revision'
    && File.Set.equal files_at_rev t.files_at_rev
  ;;

  let repository_id { revision = Rev rev; repo = _; files_at_rev = _ } =
    Repository_id.of_git_hash rev
  ;;
end

module Remote = struct
  type nonrec t =
    { repo : t
    ; handle : string
    ; default_branch : string
    }

  type uninit = t

  let update ({ repo; handle; default_branch = _ } as t) =
    let+ () = run repo [ "fetch"; handle; "--no-tags" ] in
    t
  ;;

  let don't_update t = t
  let default_branch { repo = _; handle = _; default_branch } = default_branch

  let equal { repo; handle; default_branch } t =
    equal repo t.repo
    && String.equal handle t.handle
    && String.equal default_branch t.default_branch
  ;;

  let files_at_rev repo (Rev rev) =
    run_capture_zero_separated_lines repo [ "ls-tree"; "-z"; "--long"; "-r"; rev ]
    >>| File.Set.of_list_map ~f:File.parse
  ;;

  let rev_of_name { repo; handle; default_branch = _ } ~name =
    (* TODO handle non-existing name *)
    let* rev = run_capture_line repo [ "rev-parse"; sprintf "%s/%s" handle name ] in
    let revision = Rev rev in
    let+ files_at_rev = files_at_rev repo revision in
    Some { At_rev.repo; revision = Rev rev; files_at_rev }
  ;;

  let rev_of_repository_id { repo; handle = _; default_branch = _ } repo_id =
    match Repository_id.git_hash repo_id with
    | None -> Fiber.return None
    | Some rev ->
      run_capture_line repo [ "cat-file"; "-t"; rev ]
      >>= (function
       | "commit" ->
         let revision = Rev rev in
         let+ files_at_rev = files_at_rev repo revision in
         Some { At_rev.repo; revision = Rev rev; files_at_rev }
       | _ -> Fiber.return None)
  ;;
end

let remote_exists dir ~name =
  let stderr_to = make_stderr () in
  let command = [ "remote"; "--verbose" ] in
  let+ lines =
    let git = Lazy.force Vcs.git in
    Process.run_capture_lines ~dir ~display ~stderr_to Strict git command
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
    let+ lines = run_capture_lines t [ "ls-remote"; "--symref"; source ] in
    List.find_map lines ~f:(fun line ->
      match Re.exec_opt re line with
      | None -> None
      | Some m -> Some (Re.Group.get m 1))
;;

let read_head_branch =
  let headline = Re.(compile @@ seq [ bol; rep space; str "Remote branch:" ]) in
  fun t handle ->
    let+ lines = run_capture_lines t [ "remote"; "show"; "-n"; handle ] in
    let rec inspect = function
      | [] | [ _ ] -> None
      | heading :: branch :: rest ->
        (match Re.exec_opt headline heading with
         | None -> inspect (branch :: rest)
         | Some _ -> Some (String.trim branch))
    in
    inspect lines
;;

let add_repo ({ dir } as t) ~source =
  (* TODO add this directly using .git/config *)
  let handle = source |> Dune_digest.string |> Dune_digest.to_string in
  let lock = lock_path t in
  with_flock lock ~f:(fun () ->
    let* exists = remote_exists dir ~name:handle in
    let+ default_branch =
      match exists with
      | true ->
        let+ head_branch = read_head_branch t handle in
        (match head_branch with
         | Some head_branch -> head_branch
         | None ->
           (* the rev store is in some sort of unexpected state *)
           Code_error.raise
             (sprintf "Could not load default branch of repository '%s'" source)
             [ "source", Dyn.string source; "handle", Dyn.string handle ])
      | false ->
        let* head_branch = query_head_branch t source in
        let head_branch =
          match head_branch with
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
        let+ () = run t [ "remote"; "add"; "--track"; head_branch; handle; source ] in
        head_branch
    in
    { Remote.repo = t; handle; default_branch })
;;

let content_of_files t files =
  match files with
  | [] -> Fiber.return []
  | _ :: _ ->
    let+ out =
      List.map files ~f:(fun (file : File.t) -> `Object file.hash)
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
        let acc = String.sub out ~pos ~len:file.size :: acc in
        loop acc (pos + file.size) files
    in
    List.rev (loop [] 0 files)
;;
