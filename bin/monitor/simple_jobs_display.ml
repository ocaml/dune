open Import
module Job = Dune_rpc.Job

type state = Rpc_running_jobs.State.t

let state : Rpc_running_jobs.State.t option ref = ref None

let render (new_state : Rpc_running_jobs.State.t) =
  state := Some new_state;
  Console.Status_line.refresh ()
;;

let done_status_line ~complete ~remaining ~failed =
  let number_of_jobs, jobs_info =
    match !state with
    | None -> 0, []
    | Some state ->
      let jobs =
        Rpc_running_jobs.Job_map.to_list_map
          state.displayed_jobs_by_id
          ~f:(fun (_ : Job.Id.t) displayed_job -> displayed_job.row, displayed_job)
        |> Int.Map.of_list_exn
      in
      let messages =
        (* CR-someday amokhov: We should highlight jobs that were running for longer than
           5s in red. Maybe add some other colours in the mix for different job durations.
        *)
        (* CR-someday amokhov: We could also produce a "leaderboard": a dynamically
           populated list of most expensive jobs so far. *)
        (* CR-someday amokhov: Use action decriptions from Jane Street rules when
           available. *)
        (* CR-someday amokhov: Job descriptions currently contain the [_build/default]
           prefix, which is unnecessary. Let's minimise the noise. *)
        (* CR-someday amokhov: Job descriptions currently include anonymous actions that
           are hard to connect with actual rules. Can we use Dune's alias descriptions
           instead? *)
        let render_job
          { Rpc_running_jobs.State.Displayed_job.status
          ; job
          ; row = _
          ; time_first_displayed = _
          }
          =
          let duration =
            match status with
            | `Running -> Unix.gettimeofday () -. Job.started_at job
            | `Finished_at finished_at -> finished_at -. Job.started_at job
          in
          let display =
            Pp.concat
              [ Pp.map_tags ~f:(fun () -> User_message.Style.Id) (Job.description job)
              ; Pp.textf " (%d) (%.1fs)" (Job.pid job) duration
              ]
          in
          match status with
          | `Running -> display
          | `Finished_at _ -> Pp.concat [ display; Pp.text "- finished" ]
        in
        match Int.Map.max_binding jobs with
        | None -> []
        | Some (max_row, _) ->
          List.init (max_row + 1) ~f:(fun row ->
            match Int.Map.find jobs row with
            | None -> Pp.text ""
            | Some displayed_job -> render_job displayed_job)
      in
      Rpc_running_jobs.Job_map.cardinal state.running_jobs_by_id, messages
  in
  Pp.concat
    ~sep:Pp.newline
    (Pp.textf
       "Done: %d%% (%d/%d, %d left%s) (jobs: %d)"
       (if complete + remaining = 0 then 0 else complete * 100 / (complete + remaining))
       complete
       (complete + remaining)
       remaining
       (match failed with
        | 0 -> ""
        | failed -> sprintf ", %d failed" failed)
       number_of_jobs
     :: jobs_info)
;;
