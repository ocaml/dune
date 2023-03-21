(* Monorepo benchmark runner *)

(* Run a program on a list of arguments *)
let run_blocking program args =
  Printf.printf "running: %s %s\n" program (String.concat " " args);
  let args_arr = Array.of_list (program :: args) in
  let child_pid =
    Unix.create_process program args_arr Unix.stdin Unix.stdout Unix.stderr
  in
  let got_pid, status = Unix.waitpid [] child_pid in
  if got_pid <> child_pid then failwith "wait returned unexpected pid";
  match status with
  | Unix.WEXITED _status -> ()
  | _ ->
    let command_string = String.concat " " (program :: args) in
    failwith (Printf.sprintf "`%s` unexpected process status" command_string)

(* Run a program on a list of arguments, returning the wallclock duration of
   the program in seconds. *)
let time_run_blocking program args =
  let timestamp_before = Unix.gettimeofday () in
  let () = run_blocking program args in
  let timestamp_after = Unix.gettimeofday () in
  timestamp_after -. timestamp_before

module Metric = struct
  type t = { string : string }

  let benchmark_run_blocking ~name program args =
    let time_secs = time_run_blocking program args in
    let string =
      Printf.sprintf {|{
  "name": "%s",
  "value": %f,
  "units": "seconds"
}|}
        name time_secs
    in
    { string }

  let as_string { string } = string
end

module Current_bench_results = struct
  type t =
    { name : string
    ; metrics : Metric.t list
    }

  let of_metrics ~name metrics = { name; metrics }

  let as_string { name; metrics } =
    let metrics_string =
      List.map Metric.as_string metrics |> String.concat ",\n"
    in
    Printf.sprintf
      {|{
  "results": [
    {
      "name": "%s",
      "metrics": [
%s
      ]
    }
  ]
}|}
      name metrics_string
end

module Patch = struct
  type t =
    { input : string
    ; directory : string
    }

  let apply_extra_args { input; directory } args =
    let input_abs = Printf.sprintf "%s/%s" (Unix.getcwd ()) input in
    run_blocking "patch"
      ([ "-p1"
       ; Printf.sprintf "--input=%s" input_abs
       ; Printf.sprintf "--directory=%s" directory
       ]
      @ args)

  let apply t = apply_extra_args t []

  let reverse t = apply_extra_args t [ "--reverse" ]
end

let run_benchmarks build_program build_args =
  let patch_core_new_function =
    { Patch.input = "bench-patches/core/core-new-function.diff"
    ; directory = "duniverse/core"
    }
  in
  let patch_core_error =
    { Patch.input = "bench-patches/core/core-error.diff"
    ; directory = "duniverse/core"
    }
  in
  let run_build_metric name =
    Metric.benchmark_run_blocking ~name build_program build_args
  in
  let build_from_scratch = run_build_metric "Build from scratch" in
  let null_build = run_build_metric "Null build" in
  Patch.apply patch_core_new_function;
  let build_after_changing_core =
    run_build_metric "Build after changing core"
  in
  Patch.reverse patch_core_new_function;
  let build_after_undo_changing_core =
    run_build_metric "Build after undoing changing core"
  in
  Patch.apply patch_core_error;
  let build_after_breaking_core =
    run_build_metric "Build after adding error to core"
  in
  Patch.reverse patch_core_error;
  let build_after_fixing_core =
    run_build_metric "Build after fixing error in core"
  in
  let metrics =
    [ build_from_scratch
    ; null_build
    ; build_after_changing_core
    ; build_after_undo_changing_core
    ; build_after_breaking_core
    ; build_after_fixing_core
    ]
  in
  let name = "Dune Monorepo Benchmark" in
  Current_bench_results.(of_metrics ~name metrics |> as_string)

let () =
  let argv = Array.to_list Sys.argv in
  let usage () = Printf.sprintf "%s <program> [<arg>, ...]" (List.hd argv) in
  match Array.to_list Sys.argv |> List.tl with
  | [] -> Printf.eprintf "Usage: %s\n" (usage ())
  | program :: args -> print_endline (run_benchmarks program args)
