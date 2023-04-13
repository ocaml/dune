(* Monorepo benchmark runner *)

(* Run a program on a list of arguments, returning the wallclock duration of
   the program in seconds. *)
let time_run_blocking program args =
  let args_arr = Array.of_list (program :: args) in
  let timestamp_before = Unix.gettimeofday () in
  let child_pid =
    Unix.create_process program args_arr Unix.stdin Unix.stdout Unix.stderr
  in
  let got_pid, status = Unix.waitpid [] child_pid in
  let timestamp_after = Unix.gettimeofday () in
  if got_pid <> child_pid then failwith "wait returned unexpected pid";
  let () =
    match status with
    | Unix.WEXITED 0 -> ()
    | _ ->
      let command_string = String.concat " " (program :: args) in
      failwith (Printf.sprintf "`%s` did not exit successfully" command_string)
  in
  timestamp_after -. timestamp_before

let current_bench_json_string ~command ~wallclock_duration_secs =
  let command_str = String.concat " " command in
  Printf.sprintf
    {|{
  "results": [
    {
      "name": "running command: %s",
      "metrics": [
        {
            "name": "wallclock duration",
            "value": %f,
            "units": "sec"
        }
      ]
    }
  ]
}|}
    command_str wallclock_duration_secs

let () =
  let argv = Array.to_list Sys.argv in
  let usage () = Printf.sprintf "%s <program> [<arg>, ...]" (List.hd argv) in
  match Array.to_list Sys.argv |> List.tl with
  | [] -> Printf.eprintf "Usage: %s\n" (usage ())
  | program :: args as command ->
    let wallclock_duration_secs = time_run_blocking program args in
    print_endline (current_bench_json_string ~command ~wallclock_duration_secs)
