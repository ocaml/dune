include Dune_threaded_console_intf
open Stdune

let make ~frames_per_second (module Base : S) : (module Dune_console.Backend) =
  let module T = struct
    let mutex = Mutex.create ()
    let finish_cv = Condition.create ()

    let state =
      { messages = Queue.create ()
      ; status_line = None
      ; finished = false
      ; finish_requested = false
      ; dirty = true
      }
    ;;

    let finish () =
      Mutex.lock mutex;
      state.dirty <- true;
      state.finish_requested <- true;
      while not state.finished do
        Condition.wait finish_cv mutex
      done;
      Mutex.unlock mutex
    ;;

    let print_user_message m =
      Mutex.lock mutex;
      state.dirty <- true;
      Queue.push state.messages m;
      Mutex.unlock mutex
    ;;

    let set_status_line sl =
      Mutex.lock mutex;
      state.dirty <- true;
      state.status_line <- sl;
      Mutex.unlock mutex
    ;;

    let print_if_no_status_line _msg = ()

    let reset () =
      Mutex.lock mutex;
      state.dirty <- true;
      Queue.clear state.messages;
      state.status_line <- None;
      Exn.protect ~f:Base.reset ~finally:(fun () -> Mutex.unlock mutex)
    ;;

    let reset_flush_history () =
      Mutex.lock mutex;
      state.dirty <- true;
      Queue.clear state.messages;
      state.status_line <- None;
      Exn.protect ~f:Base.reset_flush_history ~finally:(fun () -> Mutex.unlock mutex)
    ;;

    type source =
      | Render
      | Handle_user_events

    exception Exn of source * Exn_with_backtrace.t

    let start () =
      Base.start ();
      Dune_engine.Scheduler.spawn_thread
      @@ fun () ->
      Dune_util.Terminal_signals.unblock ();
      let last = ref (Unix.gettimeofday ()) in
      let frame_rate = 1. /. float_of_int frames_per_second in
      let cleanup exn =
        state.finished <- true;
        Option.iter exn ~f:(fun exn ->
          Dune_util.Log.info [ Pp.text "Console failed"; Exn_with_backtrace.pp exn ]);
        (match Exn_with_backtrace.try_with Base.finish with
         | Ok () -> ()
         | Error exn ->
           (* we can't log to console because we are cleaning it up and we
              borked it *)
           Dune_util.Log.log (fun () ->
             [ Pp.text "Error cleaning up console"; Exn_with_backtrace.pp exn ]));
        Condition.broadcast finish_cv;
        Mutex.unlock mutex
      in
      try
        (* This is the main event loop for a threaded backend.

           Firstly we lock our mutex, to prevent other threads from mutating our
           state. Next we ask our implementation to render the given state,
           afterwards checking if a finish was requested.

           If a finish was requested we exit the loop cleanly.

           We unlock our mutex and go into a time calculation. This calculation
           gets the current time and compares it with the last recorded time.
           This lets us compute the elapsed time.

           Next we check that the elapsed time is larger than our specified
           [frame_rate]. If this is the case then we can handle any pending user
           events and continue the loop as soon as possible.

           If we have not yet reached the [frame_rate] then we can handle user
           events and sleep for the remaining time. *)
        while true do
          Mutex.lock mutex;
          (match state.dirty with
           | false -> ()
           | true ->
             (match Base.render state with
              | () -> ()
              | exception exn ->
                let exn = Exn_with_backtrace.capture exn in
                raise_notrace (Exn (Render, exn)));
             if state.finish_requested then raise_notrace Exit;
             state.dirty <- false);
          Mutex.unlock mutex;
          let new_time =
            let now = Unix.gettimeofday () in
            let time_budget =
              let elapsed = now -. !last in
              if elapsed >= frame_rate then 0. else frame_rate -. elapsed
            in
            match Base.handle_user_events ~now ~time_budget mutex state with
            | time -> time
            | exception exn ->
              let exn = Exn_with_backtrace.capture exn in
              raise_notrace (Exn (Handle_user_events, exn))
          in
          last := new_time
        done
      with
      | Exit -> cleanup None
      | Exn (Render, exn) -> cleanup (Some exn)
      | Exn (Handle_user_events, exn) ->
        (* we try to re-acquire the mutex because we can only reach this by
           failing in [Base.handle_user_events]. and we don't know if
           [Base.handle_user_events] released the mutex. This should be
           impossible if the code is bug free. *)
        (match Mutex.lock mutex with
         | exception Sys_error _ -> ()
         | () -> ());
        cleanup (Some exn)
      | exn ->
        (* If any unexpected exceptions are encountered, we catch them, make
           sure we [cleanup] and then re-raise them. *)
        let exn = Exn_with_backtrace.capture exn in
        Mutex.lock mutex;
        cleanup (Some exn)
    ;;
  end
  in
  (module T)
;;

let progress ~frames_per_second =
  make
    ~frames_per_second
    (module struct
      include (val Dune_console.Backend.progress_no_flush)

      let render (state : state) =
        while not (Queue.is_empty state.messages) do
          print_user_message (Queue.pop_exn state.messages)
        done;
        set_status_line state.status_line;
        flush stderr
      ;;

      (* The current console doesn't react to user events so we just sleep until
         the next loop iteration. Because it doesn't react to user input, it cannot
         modify the UI state, and as a consequence doesn't need the mutex. *)
      let handle_user_events ~now ~time_budget (_ : Mutex.t) (_ : state) =
        Unix.sleepf time_budget;
        now +. time_budget
      ;;
    end)
;;
