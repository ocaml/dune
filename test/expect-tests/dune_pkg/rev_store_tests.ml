open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Rev_store = Dune_pkg.Rev_store
module Opam_repo = Dune_pkg.Opam_repo
module Vcs = Dune_vcs.Vcs

let () = Dune_tests_common.init ()

let run thunk =
  let on_event _config _event = () in
  let config : Scheduler.Config.t =
    { concurrency = 1; stats = None; print_ctrl_c_warning = false; watch_exclusions = [] }
  in
  Scheduler.Run.go config ~on_event thunk
;;

let display = Display.Quiet
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let create_repo_at dir =
  let git, git_out =
    let stdout_to = make_stdout () in
    let stderr_to = make_stdout () in
    let git = Lazy.force Vcs.git in
    let failure_mode = Process.Failure_mode.Strict in
    ( (fun args -> Process.run ~dir ~display ~stdout_to ~stderr_to failure_mode git args)
    , fun args -> Process.run_capture_line ~dir ~display ~stderr_to failure_mode git args
    )
  in
  Path.mkdir_p dir;
  let* () = git [ "init" ] in
  let entry_name = "entry" in
  let entry = Path.relative dir entry_name in
  Io.write_lines entry [ "just some content" ];
  let* () = git [ "add"; entry_name ] in
  let* () = git [ "commit"; "-m 'Initial commit'" ] in
  git_out [ "rev-parse"; "HEAD" ]
;;

let%expect_test "adding remotes" =
  let cwd = Path.External.cwd () |> Path.external_ in
  let dir = Path.relative cwd "git-repo" in
  run (fun () ->
    let* rev_store = Rev_store.load_or_create ~dir in
    let remote_path = Path.relative cwd "git-remote" in
    let* _head = create_repo_at remote_path in
    let opam_url = remote_path |> Path.to_string |> OpamUrl.parse in
    Dune_pkg.OpamUrl.resolve opam_url ~loc:Loc.none rev_store
    >>= function
    | Error _ -> Fiber.return @@ print_endline "Unable to find revision"
    | Ok r ->
      print_endline "Successfully found remote";
      Dune_pkg.OpamUrl.fetch_revision opam_url ~loc:Loc.none r rev_store
      >>| (function
       | Error _ -> print_endline "Unable to fetch revision"
       | Ok _ -> print_endline "successfully fetched revision"));
  [%expect {|
    Successfully found remote
    successfully fetched revision
     |}]
;;
