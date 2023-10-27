open Stdune
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Re = Dune_re

type t =
  { git : Path.t
  ; dir : Path.t
  }

type rev = Rev of string

let equal { git; dir } t = Path.equal git t.git && Path.equal dir t.dir
let display = Display.Quiet
let failure_mode = Process.Failure_mode.Strict
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let run { git; dir } =
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  Process.run ~dir ~display ~stdout_to ~stderr_to failure_mode git
;;

let run_capture_line { git; dir } =
  Process.run_capture_line ~dir ~display failure_mode git
;;

let run_capture_lines { git; dir } =
  Process.run_capture_lines ~dir ~display failure_mode git
;;

let run_capture_zero_separated_lines { git; dir } =
  Process.run_capture_zero_separated ~dir ~display failure_mode git
;;

let show { git; dir } (Rev rev) path =
  let open Fiber.O in
  let failure_mode = Dune_vcs.Vcs.git_accept () in
  let command = [ "show"; sprintf "%s:%s" rev (Path.to_string path) ] in
  let stderr_to = make_stderr () in
  let+ res = Process.run_capture ~dir ~display ~stderr_to failure_mode git command in
  match res with
  | Error _n -> None
  | Ok v -> Some v
;;

let load ~git ~dir = { git; dir }

let create ~git ~dir =
  let open Fiber.O in
  Path.mkdir_p dir;
  let t = load ~git ~dir in
  let+ () = run t [ "init"; "--bare" ] in
  t
;;

let load_or_create ~git ~dir =
  match Path.exists dir with
  | true -> load ~git ~dir |> Fiber.return
  | false -> create ~git ~dir
;;

module Remote = struct
  type nonrec t =
    { repo : t
    ; handle : string
    }

  let head_branch = Re.(compile (seq [ str "HEAD branch: "; group (rep1 any); eol ]))
  let update { repo; handle } = run repo [ "fetch"; handle; "--no-tags" ]

  let default_branch { repo; handle } =
    let open Fiber.O in
    let+ contents = run_capture_lines repo [ "remote"; "show"; handle ] in
    List.find_map contents ~f:(fun line ->
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
      let open Fiber.O in
      (* TODO future: cache ls-tree *)
      let+ all_files =
        run_capture_zero_separated_lines
          repo
          [ "ls-tree"; "-z"; "--name-only"; "-r"; rev ]
      in
      List.filter_map all_files ~f:(fun entry ->
        let path_entry = Path.of_string entry in
        Path.descendant path_entry ~of_:path)
    ;;

    let equal { remote; revision = Rev revision } t =
      let (Rev revision') = t.revision in
      equal remote t.remote && String.equal revision revision'
    ;;

    let repository_id { revision = Rev rev; remote = _ } = Repository_id.of_git_hash rev
  end

  let rev_of_name ({ repo; handle } as remote) ~name =
    let open Fiber.O in
    (* TODO handle non-existing name *)
    let+ rev = run_capture_line repo [ "rev-parse"; sprintf "%s/%s" handle name ] in
    Some { At_rev.remote; revision = Rev rev }
  ;;

  let rev_of_repository_id ({ repo; handle = _ } as remote) repo_id =
    let open Fiber.O in
    match Repository_id.git_hash repo_id with
    | None -> Fiber.return None
    | Some rev ->
      let+ type' = run_capture_line repo [ "cat-file"; "-t"; rev ] in
      (match type' with
       | "commit" -> Some { At_rev.remote; revision = Rev rev }
       | _ -> None)
  ;;
end

let remote_exists { git; dir } ~name =
  let open Fiber.O in
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let failure_mode = Process.Failure_mode.Return in
  let command = [ "remote"; "show"; name ] in
  let+ (), exit_code =
    Process.run ~dir ~display ~stderr_to ~stdout_to failure_mode git command
  in
  match exit_code with
  | 0 -> true
  | 128 | _ -> false
;;

let add_repo t ~source =
  let open Fiber.O in
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
