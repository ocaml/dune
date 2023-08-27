open! Stdune
open Import
module Job = Dune_rpc.Job
module Client = Dune_rpc_client.Client

module Job_map = Map.Make (struct
    include Job.Id

    let to_dyn = Dyn.opaque
  end)

(* The rate at which we advance time of existing jobs while waiting for new updates. *)
let refresh_rate_sec = 0.1

module State = struct
  module Displayed_job = struct
    type t =
      { job : Job.t
      ; row : int
      ; time_first_displayed : float
      ; mutable status : [ `Running | `Finished_at of float ]
      }
  end

  type t =
    { mutable running_jobs_by_id : Job.t Job_map.t
    ; mutable displayed_jobs_by_id : Displayed_job.t Job_map.t
    ; (* If this true, we continue with the rendering loop *)
      mutable keep_refreshing : bool
    ; mutable free_rows : Int.Set.t
    }

  let create ~max_rows =
    let free_rows = Int.Set.of_list (List.init max_rows ~f:Fun.id) in
    { running_jobs_by_id = Job_map.empty
    ; displayed_jobs_by_id = Job_map.empty
    ; keep_refreshing = true
    ; free_rows
    }
  ;;

  let request_stop t = t.keep_refreshing <- false

  (* Update the state by registering the newly started/stopped jobs. *)
  let update_state t ~(events : Job.Event.t list) =
    List.iter events ~f:(fun (event : Job.Event.t) ->
      match event with
      | Start (job : Job.t) ->
        t.running_jobs_by_id <- Job_map.add_exn t.running_jobs_by_id (Job.id job) job
      | Stop (job_id : Job.Id.t) ->
        t.running_jobs_by_id <- Job_map.remove t.running_jobs_by_id job_id;
        (match Job_map.find t.displayed_jobs_by_id job_id with
         | None -> ()
         | Some displayed_job ->
           assert (displayed_job.status = `Running);
           (* CR-someday amokhov: Let's get the finished at time from the server to make
              it more precise. *)
           (* We need the finished at time so that we can display "finished - started"
              instead of "now - started". *)
           displayed_job.status <- `Finished_at (Unix.gettimeofday ())))
  ;;

  let assign_free_rows ~now ~min_duration_sec t =
    (* Iteration proceeds from lower job IDs (i.e. earlier jobs) to higher ones. This
       ensures that long-lived jobs always have a chance to be displayed without being
       crowded out by numerous short-lived jobs, since the longest running job is the
       one with the lowest ID in [t.running_jobs_by_i]. In short, we give the longest
       running job the first opportunity to populate a free row. *)
    Job_map.iteri t.running_jobs_by_id ~f:(fun job_id job ->
      if not (Job_map.mem t.displayed_jobs_by_id job_id)
      then (
        match Int.Set.min_elt t.free_rows with
        | None -> ()
        | Some row ->
          (* CR-someday amokhov: Subtracting local time [now] and server time
             [started_at] assumes they are somehow compatible. It may be worth
             doing something smarter here to synchronise the clocks. *)
          if now -. job.started_at > min_duration_sec
          then (
            t.displayed_jobs_by_id
              <- Job_map.add_exn
                   t.displayed_jobs_by_id
                   job_id
                   { job; status = `Running; row; time_first_displayed = now };
            t.free_rows <- Int.Set.remove t.free_rows row)))
  ;;
end

module Display (M : Jobs_display_intf.S with type state = State.t) = struct
  let render (state : State.t) ~min_duration_sec ~min_display_duration_sec =
    let now = Unix.gettimeofday () in
    state.displayed_jobs_by_id
      <- Job_map.filteri
           state.displayed_jobs_by_id
           ~f:(fun (_ : Job.Id.t) { status; time_first_displayed; row; job = _ } ->
             match status with
             | `Running -> true
             | `Finished_at _ ->
               let keep_displayed =
                 now < time_first_displayed +. min_display_duration_sec
               in
               if not keep_displayed
               then state.free_rows <- Int.Set.add state.free_rows row;
               keep_displayed);
    State.assign_free_rows ~now ~min_duration_sec state;
    M.render state
  ;;

  let render jobs ~min_duration_sec ~min_display_duration_sec =
    try render jobs ~min_duration_sec ~min_display_duration_sec with
    | exn ->
      let exn = Exn_with_backtrace.capture exn in
      Format.eprintf "%a@.%!" Exn_with_backtrace.pp_uncaught exn;
      Exn_with_backtrace.reraise exn
  ;;

  let rec polling_loop state sub =
    let open Fiber.O in
    let* next = Client.Stream.next sub in
    match next with
    | None ->
      State.request_stop state;
      Fiber.return ()
    | Some events ->
      State.update_state state ~events;
      polling_loop state sub
  ;;

  let rec refresh_loop (state : State.t) ~min_duration_sec ~min_display_duration_sec =
    match state.keep_refreshing with
    | false -> Fiber.return ()
    | true ->
      let open Fiber.O in
      let* () =
        (* CR-someday rgrinberg: ideally our sleeps would be interruptible so we
           could stop waiting immediately if [keep_refreshing] is set *)
        Scheduler.sleep refresh_rate_sec
      in
      render state ~min_duration_sec ~min_display_duration_sec;
      refresh_loop state ~min_duration_sec ~min_display_duration_sec
  ;;

  let display_jobs client ~max_rows ~min_duration_sec ~min_display_duration_sec =
    let open Fiber.O in
    let* poll = Client.poll client Dune_rpc_private.Public.Sub.running_jobs in
    match poll with
    | Error e -> raise (Dune_rpc_private.Version_error.E e)
    | Ok sub ->
      let state = State.create ~max_rows in
      Fiber.fork_and_join_unit
        (fun () -> polling_loop state sub)
        (fun () -> refresh_loop state ~min_duration_sec ~min_display_duration_sec)
  ;;
end
