open Stdune
open Dune_engine
open Dune_scheduler

let go =
  let config =
    Clflags.display := Short;
    { Scheduler.Config.concurrency = 1
    ; scheduling_policy = None
    ; print_ctrl_c_warning = true
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config ~file_watcher:No_watcher
;;

let true_ = Bin.which "true" ~path:(Env_path.path Env.initial) |> Option.value_exn

let dtemp_dir purpose =
  let env = Dune_engine.For_tests.Dtemp.add_to_env Env.empty ~purpose in
  Env.get env Env.Var.temp_dir |> Option.value_exn |> Path.of_string
;;

let%expect_test "action and Dune temporary directories" =
  let dune_dir = dtemp_dir Internal_job in
  let action_dir = dtemp_dir (Build_job None) in
  let dune_file = Dune_engine.For_tests.Dtemp.file ~prefix:"dune" ~suffix:"file" in
  let action_file = Path.relative action_dir "action-file" in
  Io.write_file action_file "";
  let exists path = Fpath.exists (Path.to_string path) in
  Printf.printf "same directory: %b\n" (Path.equal dune_dir action_dir);
  Printf.printf
    "Dune file uses Dune directory: %b\n"
    (Path.equal dune_dir (Path.parent_exn dune_file));
  Printf.printf "files before clear: %b %b\n" (exists dune_file) (exists action_file);
  Dune_engine.For_tests.Dtemp.clear ();
  Printf.printf "files after clear: %b %b\n" (exists dune_file) (exists action_file);
  [%expect
    {|
    same directory: false
    Dune file uses Dune directory: true
    files before clear: true true
    files after clear: false false
    |}]
;;

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
