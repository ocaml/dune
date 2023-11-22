open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Rev_store = Dune_pkg.Rev_store
module Vcs = Dune_vcs.Vcs

let () = Dune_tests_common.init ()

let run thunk =
  let on_event _config _event = () in
  let config : Scheduler.Config.t =
    { concurrency = 1
    ; stats = None
    ; insignificant_changes = `Ignore
    ; signal_watcher = `No
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config ~on_event thunk
;;

let display = Display.Quiet
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let create_repo_at dir =
  let stdout_to = make_stdout () in
  let stderr_to = make_stdout () in
  let git =
    let git = Lazy.force Vcs.git in
    Process.run ~dir ~display ~stdout_to ~stderr_to Process.Failure_mode.Strict git
  in
  Path.mkdir_p dir;
  let* () = git [ "init" ] in
  let entry_name = "entry" in
  let entry = Path.relative dir entry_name in
  Io.write_lines entry [ "just some content" ];
  let* () = git [ "add"; entry_name ] in
  git [ "commit"; "-m 'Initial commit'" ]
;;

let%expect_test "adding remotes" =
  let cwd = Path.External.cwd () |> Path.external_ in
  let dir = Path.relative cwd "git-repo" in
  run (fun () ->
    let* rev_store = Rev_store.load_or_create ~dir in
    let remote_path = Path.relative cwd "git-remote" in
    let* () = create_repo_at remote_path in
    let source = Path.to_string remote_path in
    let* remote = Rev_store.add_repo rev_store ~source in
    let* (_ : Rev_store.Remote.t) = Rev_store.Remote.update remote in
    print_endline "Creating first remote succeeded";
    [%expect {|
    Creating first remote succeeded
    |}];
    let* (_remote' : Rev_store.Remote.uninit) = Rev_store.add_repo rev_store ~source in
    print_endline "Adding same remote without update succeeded";
    [%expect {|
    Adding same remote without update succeeded
    |}];
    let* remote'' = Rev_store.add_repo rev_store ~source in
    let* (_ : Rev_store.Remote.t) = Rev_store.Remote.update remote'' in
    print_endline "Adding same remote with update succeeded";
    [%expect {|
    Adding same remote with update succeeded
    |}];
    Fiber.return ())
;;
