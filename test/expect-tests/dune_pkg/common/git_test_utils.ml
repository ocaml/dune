open Stdune
open Dune_scheduler
open Fiber.O
module Process = Dune_engine.Process
module Display = Dune_engine.Display
module Vcs = Dune_vcs.Vcs

let run thunk =
  let on_event _config _event = () in
  let config : Scheduler.Config.t =
    { concurrency = 1; print_ctrl_c_warning = false; watch_exclusions = [] }
  in
  Scheduler.Run.go config ~on_event thunk
;;

let display = Display.Quiet
let output_limit = Sys.max_string_length
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

let git ~dir =
  let stdout_to = make_stdout () in
  let stderr_to = make_stdout () in
  let git = Lazy.force Vcs.git in
  let failure_mode = Process.Failure_mode.Strict in
  fun args -> Process.run ~dir ~display ~stdout_to ~stderr_to failure_mode git args
;;

let git_out ~dir =
  let stderr_to = make_stdout () in
  let git = Lazy.force Vcs.git in
  let failure_mode = Process.Failure_mode.Strict in
  fun args -> Process.run_capture_line ~dir ~display ~stderr_to failure_mode git args
;;

let git_init_and_config_user dir =
  Path.mkdir_p dir;
  let git = git ~dir in
  git [ "init" ]
  >>> git [ "config"; "user.name"; "\"Test Name\"" ]
  >>> git [ "config"; "user.email"; "\"test@example.com\"" ]
;;

let create_repo_at dir =
  let git = git ~dir in
  let* () = git_init_and_config_user dir in
  let entry_name = "entry" in
  let entry = Path.relative dir entry_name in
  Io.write_lines entry [ "just some content" ];
  git [ "add"; entry_name ]
  >>> git [ "commit"; "-m 'Initial commit'" ]
  >>> git_out ~dir [ "rev-parse"; "HEAD" ]
;;
