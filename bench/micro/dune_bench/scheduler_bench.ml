(* Benchmark the scheduler *)

open Stdune
open Dune_engine
module Caml = Stdlib

let config =
  { Scheduler.Config.concurrency = 1
  ; display = { verbosity = Short; status_line = false }
  ; stats = None
  ; insignificant_changes = `React
  ; signal_watcher = `No
  }

let setup =
  lazy
    (Path.set_root (Path.External.cwd ());
     Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build"))

let prog = Option.value_exn (Bin.which ~path:(Env.path Env.initial) "true")

let run () = Process.run ~env:Env.initial Strict prog []

let go ~jobs fiber =
  Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    { config with concurrency = jobs }
    fiber

let%bench_fun "single" =
  Lazy.force setup;
  fun () -> go run ~jobs:1

let l = List.init 100 ~f:ignore

let%bench_fun ("many" [@indexed jobs = [ 1; 2; 4; 8 ]]) =
  Lazy.force setup;
  fun () -> go ~jobs (fun () -> Fiber.parallel_iter l ~f:run)
