open Import
open Fiber.O
module Client = Dune_rpc_client.Client
module Version_error = Dune_rpc_private.Version_error

include struct
  open Dune_rpc
  module Diagnostic = Diagnostic
  module Progress = Progress
end

module Diagnostic_map = Map.Make (struct
    include Diagnostic.Id

    let to_dyn = Dyn.opaque
  end)

module Event = struct
  (** Events that the render loop will process. *)
  type t =
    | Diagnostics of Diagnostic.Event.t list
    | Progress of Progress.t
end

module State : sig
  (** Internal state of the render loop. *)
  type t

  (** Initial empty state. *)
  val init
    :  done_status_line:
         (complete:int -> remaining:int -> failed:int -> User_message.Style.t Pp.t)
    -> unit
    -> t

  module Update : sig
    (** Incremental updates to the state. Computes increments of the state that
        will be used for efficient rendering. *)
    type t
  end

  val update : t -> Event.t -> Update.t

  (** Given a state update, render the update. *)
  val render : t -> Update.t -> unit
end = struct
  type t =
    { mutable diagnostics : Diagnostic.t Diagnostic_map.t
    ; mutable progress : Progress.t
    ; done_status_line :
        complete:int -> remaining:int -> failed:int -> User_message.Style.t Pp.t
    }

  let init ~done_status_line () =
    { diagnostics = Diagnostic_map.empty; progress = Waiting; done_status_line }
  ;;

  let waiting_for_file_system_changes message =
    Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")
  ;;

  let restarting_current_build message =
    Pp.seq message (Pp.verbatim ", restarting current build...")
  ;;

  let had_errors state =
    match Diagnostic_map.cardinal state.diagnostics with
    | 1 -> Pp.verbatim "Had 1 error"
    | n -> Pp.textf "Had %d errors" n
  ;;

  let status (state : t) =
    Console.Status_line.set
      (Live
         (fun () ->
           match (state.progress : Progress.t) with
           | Waiting -> Pp.verbatim "Initializing..."
           | In_progress { complete; remaining; failed } ->
             state.done_status_line ~complete ~remaining ~failed
           | Interrupted ->
             Pp.tag User_message.Style.Error (Pp.verbatim "Source files changed")
             |> restarting_current_build
           | Success ->
             Pp.tag User_message.Style.Success (Pp.verbatim "Success")
             |> waiting_for_file_system_changes
           | Failed ->
             Pp.tag User_message.Style.Error (had_errors state)
             |> waiting_for_file_system_changes))
  ;;

  module Update = struct
    type t =
      | Update_status
      | Add_diagnostics of Diagnostic.t list
      | Refresh

    let progress state progress =
      state.progress <- progress;
      Update_status
    ;;

    let diagnostics state diagnostics =
      let mode, diagnostics =
        List.fold_left
          diagnostics
          ~init:(`Add_only [], state.diagnostics)
          ~f:(fun (mode, acc) diag_event ->
            match (diag_event : Diagnostic.Event.t) with
            | Remove diag -> `Remove, Diagnostic_map.remove acc diag.id
            | Add diag ->
              ( (match mode with
                 | `Add_only diags -> `Add_only (diag :: diags)
                 | `Remove -> `Remove)
              , (match Diagnostic_map.add acc diag.id diag with
                 | Ok map -> map
                 | Error _ -> acc) ))
      in
      state.diagnostics <- diagnostics;
      match mode with
      | `Add_only update -> Add_diagnostics (List.rev update)
      | `Remove -> Refresh
    ;;
  end

  let update state (event : Event.t) =
    match event with
    | Progress progress -> Update.progress state progress
    | Diagnostics diagnostics -> Update.diagnostics state diagnostics
  ;;

  let render =
    let f d = Console.print_user_message (Diagnostic.to_user_message d) in
    fun (state : t) (update : Update.t) ->
      (match (update : Update.t) with
       | Add_diagnostics diags -> List.iter diags ~f
       | Update_status -> ()
       | Refresh ->
         Console.reset ();
         Diagnostic_map.iter state.diagnostics ~f);
      status state
  ;;
end

(* A generic loop that continuously fetches events from a [sub] that it opens a poll to
   and writes them to the [event] bus. *)
let fetch_loop ~(event : Event.t Fiber_event_bus.t) ~client ~f sub =
  Client.poll client sub
  >>= function
  | Error version_error ->
    let* () = Fiber_event_bus.close event in
    User_error.raise [ Pp.verbatim (Version_error.message version_error) ]
  | Ok poller ->
    let rec loop () =
      Fiber.collect_errors (fun () -> Client.Stream.next poller)
      >>= (function
            | Ok (Some payload) -> Fiber_event_bus.push event (f payload)
            | Error _ | Ok None -> Fiber_event_bus.close event >>> Fiber.return `Closed)
      >>= function
      | `Closed -> Fiber.return ()
      | `Ok -> loop ()
    in
    loop ()
;;

(* Main render loop *)
let render_loop state ~(event : Event.t Fiber_event_bus.t) =
  Console.reset ();
  let rec loop () =
    Fiber_event_bus.pop event
    >>= function
    | `Closed ->
      Console.print_user_message
        (User_error.make [ Pp.textf "Lost connection to server." ]);
      Fiber.return ()
    | `Next event ->
      let update = State.update state event in
      (* CR-someday alizter: If performance of rendering here on every loop is bad we can
         instead batch updates. It should be very simple to write a [State.Update.union]
         function that can combine incremental updates to be done at once. *)
      State.render state update;
      loop ()
  in
  loop ()
;;

type monitor_options =
  { quit_on_disconnect : bool
  ; jobs_options : jobs_options
  }

and jobs_options =
  { jobs_display : (module Jobs_display_intf.S with type state = Rpc_running_jobs.State.t)
  ; max_rows : int
  ; min_duration_sec : float
  ; min_display_duration_sec : float
  }

let monitor
  { quit_on_disconnect
  ; jobs_options = { jobs_display; max_rows; min_duration_sec; min_display_duration_sec }
  }
  ()
  =
  Fiber.with_error_handler ~on_error:(fun exn ->
    Dune_util.Log.log (fun () -> [ Exn_with_backtrace.pp exn ]);
    Fiber.never)
  @@ fun () ->
  Fiber.repeat_while ~init:1 ~f:(fun i ->
    match Dune_rpc_impl.Where.get () with
    | Some where ->
      let* connect = Client.Connection.connect where in
      (match connect with
       | Error msg ->
         User_message.print msg;
         Fiber.return (Some i)
       | Ok connect ->
         let+ () =
           Dune_rpc_impl.Client.client
             connect
             (Dune_rpc.Initialize.Request.create
                ~id:(Dune_rpc.Id.make (Sexp.Atom "monitor_cmd")))
             ~f:(fun client ->
               let event = Fiber_event_bus.create () in
               let module Jobs_display = (val jobs_display) in
               let state =
                 State.init ~done_status_line:Jobs_display.done_status_line ()
               in
               let module Jobs_display = Rpc_running_jobs.Display (Jobs_display) in
               Fiber.all_concurrently_unit
                 [ render_loop ~event state
                 ; fetch_loop
                     ~event
                     ~client
                     ~f:(fun x -> Event.Progress x)
                     Dune_rpc.Public.Sub.progress
                 ; fetch_loop
                     ~event
                     ~client
                     ~f:(fun x -> Event.Diagnostics x)
                     Dune_rpc.Public.Sub.diagnostic
                 ; Jobs_display.display_jobs
                     client
                     ~max_rows
                     ~min_duration_sec
                     ~min_display_duration_sec
                 ])
         in
         Some i)
    | None when quit_on_disconnect ->
      User_error.raise [ Pp.text "RPC server not running." ]
    | None ->
      Console.Status_line.set
        (Console.Status_line.Live
           (fun () -> Pp.verbatim ("Waiting for RPC server" ^ String.make (i mod 4) '.')));
      let+ () = Scheduler.sleep 0.3 in
      Some (i + 1))
;;

let man =
  [ `S "DESCRIPTION"
  ; `P
      "$(b,dune monitor) connects to an RPC server running in the current workspace and \
       displays the build progress and diagnostics. If no server is running or it was \
       disconnected, it will continuously try to reconnect."
  ; `P
      "When $(b,--display tui) is passed, it also displays the running jobs together \
       with their status and timing information."
  ]
;;

let command =
  let info =
    let doc = "Connect to a Dune RPC server and monitor it." in
    Cmd.info "monitor" ~doc ~man
  and term =
    let open Import in
    let+ (common : Common.t) = Common.term
    and+ quit_on_disconnect =
      Arg.(
        value
        & flag
        & info
            [ "quit-on-disconnect" ]
            ~doc:"Quit if the connection to the server is lost.")
    in
    let common = Common.forbid_builds common in
    let log_file =
      Log.File.This
        (Path.append_local Path.build_dir (Path.Local.of_string ".monitor.log"))
    in
    let config = Common.init ~log_file common in
    let stats = Common.stats common in
    let display = config.display in
    let config =
      Dune_config.for_scheduler
        config
        stats
        ~insignificant_changes:`Ignore
        ~signal_watcher:`Yes
        ~watch_exclusions:[]
    in
    Scheduler.Run.go
      config
      ~on_event:(fun _ _ -> ())
      ~file_watcher:No_watcher
      (monitor
         { quit_on_disconnect
         ; jobs_options =
             (* CR-someday alizter: make these configurable. *)
             { jobs_display =
                 (match display with
                  | Tui -> (module Tui_jobs_display)
                  | Simple _ -> (module Simple_jobs_display))
             ; max_rows =
                 10
                 (* CR-someday alizter: give max_rows a better default, specific to display. *)
             ; min_duration_sec = 0.5
             ; min_display_duration_sec = 3.0
             }
         })
  in
  Cmd.v info term
;;
