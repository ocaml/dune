open Import
open Dune_tui.Import
module Job = Dune_rpc.Job

type state = Rpc_running_jobs.State.t

let number_of_jobs = Lwd.var 0

let render (state : Rpc_running_jobs.State.t) =
  let now = Unix.gettimeofday () in
  let jobs =
    Rpc_running_jobs.Job_map.to_list_map
      state.displayed_jobs_by_id
      ~f:(fun (_ : Job.Id.t) displayed_job -> displayed_job.row, displayed_job)
    |> Int.Map.of_list_exn
  in
  (* CR-someday amokhov: We should highlight jobs that were running for longer than 5s
     in red. Maybe add some other colours in the mix for different job durations. *)
  (* CR-someday amokhov: We could also produce a "leaderboard": a dynamically
     populated list of most expensive jobs so far. *)
  (* CR-someday amokhov: Job descriptions currently contain the [_build/default]
     prefix, which is unnecessary. Let's minimise the noise. *)
  (* CR-someday amokhov: Job descriptions currently include anonymous actions that are
     hard to connect with actual rules. Can we use Dune's alias descriptions instead?
  *)
  let render_job
    { Rpc_running_jobs.State.Displayed_job.status
    ; job
    ; row = _
    ; time_first_displayed = _
    }
    =
    let duration =
      match status with
      | `Running -> now -. Job.started_at job
      | `Finished_at finished_at -> finished_at -. Job.started_at job
    in
    let status =
      (match status with
       | `Running -> "Running"
       | `Finished_at _ -> "Finished")
      |> I.string A.empty
      |> Ui.atom
    in
    let pid = Job.pid job |> I.strf "%d" |> Ui.atom in
    let duration = I.strf "%.1f" duration |> Ui.atom in
    let description =
      Job.description job
      |> Pp.map_tags ~f:(fun () -> User_message.Style.Id)
      |> Dune_tui.Drawing.pp_to_image
      |> Ui.atom
    in
    status, pid, duration, description
  in
  let jobs =
    match Int.Map.max_binding jobs with
    | None -> Ui.empty
    | Some (max_row, _) ->
      let rows =
        List.init (max_row + 1) ~f:(fun row ->
          match Int.Map.find jobs row with
          | None -> Ui.empty, Ui.empty, Ui.empty, Ui.empty
          | Some displayed_job -> render_job displayed_job)
      in
      (* CR-someday alizter: Abstract away the table drawing into a generic GUI
         widget. The titles can also be buttons enforcing an ordering on the rows. *)
      let rows =
        ( Ui.atom (I.string A.empty "Status")
        , Ui.atom (I.string A.empty "PID")
        , Ui.atom (I.string A.empty "Duration")
        , Ui.atom (I.string A.empty "Description") )
        :: rows
      in
      let bar =
        Ui.atom @@ Dune_tui.Drawing.vertical_rule ~attr:A.empty ~h:(List.length rows)
      in
      (* CR-someday alizter: do these list manipulations more efficiently. *)
      (* We concatenate the columns first so that they are spaced out evenly. *)
      Ui.hcat
        [ List.map rows ~f:(fun (x, _, _, _) -> x) |> Ui.vcat
        ; bar
        ; List.map rows ~f:(fun (_, x, _, _) -> x) |> Ui.vcat
        ; bar
        ; List.map rows ~f:(fun (_, _, x, _) -> x) |> Ui.vcat
        ; bar
        ; List.map rows ~f:(fun (_, _, _, x) -> x) |> Ui.vcat
        ]
  in
  Lwd.set number_of_jobs (Rpc_running_jobs.Job_map.cardinal state.running_jobs_by_id);
  let open Lwd.O in
  let+ number_of_jobs = Lwd.get number_of_jobs in
  Ui.vcat [ Ui.atom (I.string A.empty (sprintf "Running %d jobs" number_of_jobs)); jobs ]
;;

let render =
  let scroll_state = Lwd.var Dune_tui.Widgets.Scrollbox.State.init in
  fun (state : Rpc_running_jobs.State.t) ->
    let open Lwd.O in
    let ui () =
      let+ { ui; vscroll; hscroll } =
        render state >>= Dune_tui.Widgets.Scrollbox.make scroll_state
      in
      let keyboard_handler : Ui.key -> Ui.may_handle = function
        (* Arrow keys and vim bindings can also scroll *)
        | (`Arrow `Down | `ASCII 'j'), _ ->
          vscroll ~dir:`Down;
          `Handled
        | (`Arrow `Up | `ASCII 'k'), _ ->
          vscroll ~dir:`Up;
          `Handled
        | (`Arrow `Left | `ASCII 'h'), _ ->
          hscroll ~dir:`Left;
          `Handled
        | (`Arrow `Right | `ASCII 'l'), _ ->
          hscroll ~dir:`Right;
          `Handled
        | _ -> `Unhandled
      in
      let mouse_handler ~x:_ ~y:_ = function
        | `Scroll dir ->
          vscroll ~dir;
          `Handled
        | _ -> `Unhandled
      in
      ui |> Ui.keyboard_area keyboard_handler |> Ui.mouse_area mouse_handler
    in
    { Dune_tui.Widgets.Tabs.Tab.ui; title = "Jobs" } |> Dune_tui.update_tabs 1;
    Console.Status_line.refresh ()
;;

let done_status_line ~complete ~remaining ~failed =
  Pp.textf
    "Done: %d%% (%d/%d, %d left%s) (jobs: %d)"
    (if complete + remaining = 0 then 0 else complete * 100 / (complete + remaining))
    complete
    (complete + remaining)
    remaining
    (match failed with
     | 0 -> ""
     | failed -> sprintf ", %d failed" failed)
    (Lwd.peek number_of_jobs)
;;
