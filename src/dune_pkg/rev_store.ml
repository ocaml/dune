open Stdune
open Dune_vcs
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Re = Dune_re
open Fiber.O

type t = { dir : Path.t }
type rev = Rev of string

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

let show { dir } (Rev rev) path =
  let git = Lazy.force Vcs.git in
  let failure_mode = Vcs.git_accept () in
  let command = [ "show"; sprintf "%s:%s" rev (Path.Local.to_string path) ] in
  let stderr_to = make_stderr () in
  Process.run_capture ~dir ~display ~stderr_to failure_mode git command
  >>| Result.to_option
;;

let load_or_create ~dir =
  let t = { dir } in
  let* () = Fiber.return () in
  let+ () =
    match Fpath.mkdir_p (Path.to_string dir) with
    | Already_exists -> Fiber.return ()
    | Created -> run t [ "init"; "--bare" ]
    | Already_exists_not_directory dir ->
      User_error.raise
        [ Pp.textf "%s isn't a directory" dir ]
        ~hints:[ Pp.text "delete this file or check its permissions" ]
  in
  t
;;

module Remote = struct
  type nonrec t =
    { repo : t
    ; handle : string
    }

  let head_branch = Re.(compile (seq [ str "HEAD branch: "; group (rep1 any); eol ]))
  let update { repo; handle } = run repo [ "fetch"; handle; "--no-tags" ]

  let default_branch { repo; handle } =
    run_capture_lines repo [ "remote"; "show"; handle ]
    >>| List.find_map ~f:(fun line ->
      Re.exec_opt head_branch line |> Option.map ~f:(fun groups -> Re.Group.get groups 1))
  ;;

  let equal { repo; handle } t = equal repo t.repo && String.equal handle t.handle

  module At_rev = struct
    type nonrec t =
      { remote : t
      ; revision : rev
      }

    let content { remote = { repo; handle = _ }; revision } path = show repo revision path

    let directory_entries { remote = { repo; handle = _ }; revision = Rev rev } path =
      (* TODO: there are much better of implementing this:
         1. Using one [$ git show] for the entire director
         2. using libgit or ocamlgit
         3. using [$ git archive] *)
      let+ all_files =
        run_capture_zero_separated_lines
          repo
          [ "ls-tree"; "-z"; "--name-only"; "-r"; rev ]
      in
      List.filter_map all_files ~f:(fun entry ->
        let path_entry = Path.Local.of_string entry in
        Option.some_if (Path.Local.is_descendant path_entry ~of_:path) path_entry)
    ;;

    let equal { remote; revision = Rev revision } t =
      let (Rev revision') = t.revision in
      equal remote t.remote && String.equal revision revision'
    ;;

    let repository_id { revision = Rev rev; remote = _ } = Repository_id.of_git_hash rev
  end

  let rev_of_name ({ repo; handle } as remote) ~name =
    (* TODO handle non-existing name *)
    let+ rev = run_capture_line repo [ "rev-parse"; sprintf "%s/%s" handle name ] in
    Some { At_rev.remote; revision = Rev rev }
  ;;

  let rev_of_repository_id ({ repo; handle = _ } as remote) repo_id =
    match Repository_id.git_hash repo_id with
    | None -> Fiber.return None
    | Some rev ->
      run_capture_line repo [ "cat-file"; "-t"; rev ]
      >>| (function
      | "commit" -> Some { At_rev.remote; revision = Rev rev }
      | _ -> None)
  ;;
end

let remote_exists { dir } ~name =
  (* TODO read this directly from .git/config *)
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let command = [ "remote"; "show"; name ] in
  let+ (), exit_code =
    let git = Lazy.force Vcs.git in
    Process.run ~dir ~display ~stderr_to ~stdout_to Return git command
  in
  match exit_code with
  | 0 -> true
  | 128 | _ -> false
;;

let add_repo t ~source =
  (* TODO add this directly using .git/config *)
  let handle = source |> Dune_digest.string |> Dune_digest.to_string in
  let* exists = remote_exists t ~name:handle in
  let* () =
    match exists with
    | true -> Fiber.return ()
    | false -> run t [ "remote"; "add"; handle; source ]
  in
  let remote : Remote.t = { repo = t; handle } in
  let+ () = Remote.update remote in
  remote
;;
