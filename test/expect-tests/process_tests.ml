open Stdune
open! Dune_tests_common
open Dune_engine

let go =
  let config =
    Clflags.display := Short;
    { Scheduler.Config.concurrency = 1
    ; stats = None
    ; print_ctrl_c_warning = true
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config ~file_watcher:No_watcher ~on_event:(fun _ _ -> ())
;;

let true_ = Bin.which "true" ~path:(Env_path.path Env.initial) |> Option.value_exn

let%expect_test "null input" =
  let stdin_from = Process.(Io.null In) in
  let run () = Process.run ~display:Quiet ~stdin_from Strict true_ [] in
  let _res = go run in
  [%expect {||}]
;;

let%expect_test "null output" =
  let stdout_to = Process.(Io.null Out) in
  let stderr_to = Process.(Io.null Out) in
  let run () = Process.run ~display:Quiet ~stdout_to ~stderr_to Strict true_ [] in
  let _res = go run in
  [%expect {||}]
;;
