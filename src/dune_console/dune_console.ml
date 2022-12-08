open Stdune

module Backend = struct
  module type S = sig
    val start : unit -> unit

    val print_user_message : User_message.t -> unit

    val set_status_line : User_message.Style.t Pp.t option -> unit

    val print_if_no_status_line : User_message.Style.t Pp.t -> unit

    val reset : unit -> unit

    val reset_flush_history : unit -> unit

    val finish : unit -> unit
  end

  type t = (module S)

  module Dumb_no_flush : S = struct
    let start () = ()

    let finish () = ()

    let print_user_message msg =
      Option.iter msg.User_message.loc ~f:(fun loc ->
          Loc.render Format.err_formatter (Loc.pp loc));
      User_message.prerr { msg with loc = None }

    let set_status_line _ = ()

    let print_if_no_status_line msg =
      (* [Pp.cut] seems to be enough to force the terminating newline to
         appear. *)
      Ansi_color.prerr
        (Pp.seq (Pp.map_tags msg ~f:User_message.Print_config.default) Pp.cut)

    let reset () = prerr_string "\x1b[H\x1b[2J"

    let reset_flush_history () = prerr_string "\x1b[1;1H\x1b[2J\x1b[3J"
  end

  module Dumb : S = struct
    include Dumb_no_flush

    let print_if_no_status_line msg =
      print_if_no_status_line msg;
      flush stderr

    let print_user_message msg =
      print_user_message msg;
      flush stderr

    let reset () =
      reset ();
      flush stderr

    let reset_flush_history () =
      reset_flush_history ();
      flush stderr
  end

  module Progress_no_flush : S = struct
    let status_line = ref Pp.nop

    let start () = ()

    let status_line_len = ref 0

    let hide_status_line () =
      if !status_line_len > 0 then Printf.eprintf "\r%*s\r" !status_line_len ""

    let show_status_line () =
      if !status_line_len > 0 then Ansi_color.prerr !status_line

    let set_status_line = function
      | None ->
        hide_status_line ();
        status_line := Pp.nop;
        status_line_len := 0
      | Some line ->
        let line = Pp.map_tags line ~f:User_message.Print_config.default in
        let line_len = String.length (Format.asprintf "%a" Pp.to_fmt line) in
        hide_status_line ();
        status_line := line;
        status_line_len := line_len;
        show_status_line ()

    let print_if_no_status_line _msg = ()

    let print_user_message msg =
      hide_status_line ();
      Dumb_no_flush.print_user_message msg;
      show_status_line ()

    let reset () = Dumb.reset ()

    let finish () = set_status_line None

    let reset_flush_history () = Dumb.reset_flush_history ()
  end

  type state =
    { messages : User_message.t Queue.t
    ; mutable finish_requested : bool
    ; mutable finished : bool
    ; mutable status_line : User_message.Style.t Pp.t option
    }

  module Tui () = struct
    module Term = Notty_unix.Term

    let term = Term.create ~nosig:false ()

    let start () = Unix.set_nonblock Unix.stdin

    let image ~status_line ~messages =
      let status =
        match (status_line : User_message.Style.t Pp.t option) with
        | None -> []
        | Some message ->
          [ Notty_console.image_of_user_message_style_pp message ]
      in
      let messages =
        List.map messages ~f:(fun msg ->
            Notty_console.image_of_user_message_style_pp (User_message.pp msg))
      in
      Notty.I.vcat (messages @ status)

    let render state =
      let messages = Queue.to_list state.messages in
      let image = image ~status_line:state.status_line ~messages in
      Term.image term image

    let handle_user_events ~now ~timeout mutex =
      let input_fds, _, _ = Unix.select [ Unix.stdin ] [] [] timeout in
      match input_fds with
      | [] -> now +. timeout
      | _ :: _ ->
        Mutex.lock mutex;
        (try
           match Term.event term with
           | `Key (`ASCII 'q', _) -> Unix.kill (Unix.getpid ()) Sys.sigterm
           | _ -> ()
         with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ());
        Mutex.unlock mutex;
        Unix.gettimeofday ()

    let reset () = ()

    let reset_flush_history () = ()

    let finish () = Notty_unix.Term.release term
  end

  let dumb = (module Dumb : S)

  let main = ref dumb

  let set t = main := t

  let compose (module A : S) (module B : S) : (module S) =
    (module struct
      let start () =
        A.start ();
        B.start ()

      let print_user_message msg =
        A.print_user_message msg;
        B.print_user_message msg

      let set_status_line x =
        A.set_status_line x;
        B.set_status_line x

      let finish () =
        A.finish ();
        B.finish ()

      let print_if_no_status_line msg =
        A.print_if_no_status_line msg;
        B.print_if_no_status_line msg

      let reset () =
        A.reset ();
        B.reset ()

      let reset_flush_history () =
        A.reset_flush_history ();
        B.reset_flush_history ()
    end : S)

  let spawn_thread = Fdecl.create Dyn.opaque

  (** [Threaded] is the interface for user interfaces that are rendered in a
      separate thread. *)
  module type Threaded = sig
    (** [start] is called by the main thread to start broadcasting the user
        interface. Any initial setup should be performed here. *)
    val start : unit -> unit

    (** [render state] is called by the main thread to render the current state
        of the user interface. *)
    val render : state -> unit

    (** [handle_user_events ~now ~timeout mutex] is called by the main thread to
        handle user events such as keypresses. The function should return the
        time at which the next event should be handled. A [mutex] is provided in
        order to lock the state of the UI.*)
    val handle_user_events : now:float -> timeout:float -> Mutex.t -> float

    (** [reset] is called by the main thread to reset the user interface. *)
    val reset : unit -> unit

    (** [reset_flush_history] is called by the main thread to reset and flush
        the user interface. *)
    val reset_flush_history : unit -> unit

    (** [finish] is called finally by the main thread to finish broadcasting the
        user interface. Any locks on the terminal should be released here. *)
    val finish : unit -> unit
  end

  module Progress_no_flush_threaded : Threaded = struct
    include Progress_no_flush

    let render state =
      while not (Queue.is_empty state.messages) do
        print_user_message (Queue.pop_exn state.messages)
      done;
      set_status_line state.status_line;
      flush stderr

    let handle_user_events ~now ~timeout _ =
      Unix.sleepf timeout;
      now +. timeout
  end

  let threaded (module Base : Threaded) : (module S) =
    let module T = struct
      let mutex = Mutex.create ()

      let finish_cv = Condition.create ()

      let state =
        { messages = Queue.create ()
        ; status_line = None
        ; finished = false
        ; finish_requested = false
        }

      let start () =
        Mutex.lock mutex;
        Base.start ();
        Mutex.unlock mutex

      let finish () =
        Mutex.lock mutex;
        state.finish_requested <- true;
        while not state.finished do
          Condition.wait finish_cv mutex
        done;
        Mutex.unlock mutex

      let print_user_message m =
        Mutex.lock mutex;
        Queue.push state.messages m;
        Mutex.unlock mutex

      let set_status_line sl =
        Mutex.lock mutex;
        state.status_line <- sl;
        Mutex.unlock mutex

      let print_if_no_status_line _msg = ()

      let reset () =
        Mutex.lock mutex;
        Queue.clear state.messages;
        state.status_line <- None;
        Base.reset ();
        Mutex.unlock mutex

      let reset_flush_history () =
        Mutex.lock mutex;
        Queue.clear state.messages;
        state.status_line <- None;
        Base.reset_flush_history ();
        Mutex.unlock mutex
    end in
    ( Fdecl.get spawn_thread @@ fun () ->
      let open T in
      let last = ref (Unix.gettimeofday ()) in
      let frame_rate = 1. /. 60. in
      let cleanup () =
        state.finished <- true;
        Base.finish ();
        Condition.broadcast finish_cv;
        Mutex.unlock mutex
      in
      try
        Base.start ();
        while true do
          Mutex.lock mutex;
          let finish_requested = state.finish_requested in
          if finish_requested then raise_notrace Exit;
          Base.render state;
          Mutex.unlock mutex;
          let now = Unix.gettimeofday () in
          let elapsed = now -. !last in
          let new_time =
            if elapsed >= frame_rate then
              Base.handle_user_events ~now ~timeout:0.0 mutex
            else
              let delta = frame_rate -. elapsed in
              Base.handle_user_events ~now ~timeout:delta mutex
          in
          last := new_time
        done
      with
      | Exit -> cleanup ()
      | exn ->
        let exn = Exn_with_backtrace.capture exn in
        cleanup ();
        Exn_with_backtrace.reraise exn );
    (module T)

  let progress =
    let t = lazy (threaded (module Progress_no_flush_threaded)) in
    fun () -> Lazy.force t

  let tui =
    let t = lazy (threaded (module Tui ())) in
    fun () -> Lazy.force t
end

let print_user_message msg =
  let (module M : Backend.S) = !Backend.main in
  M.print_user_message msg

let print paragraphs = print_user_message (User_message.make paragraphs)

let printf fmt = Printf.ksprintf (fun msg -> print [ Pp.verbatim msg ]) fmt

let set_status_line line =
  let (module M : Backend.S) = !Backend.main in
  M.set_status_line line

let print_if_no_status_line line =
  let (module M : Backend.S) = !Backend.main in
  M.print_if_no_status_line line

let reset () =
  let (module M : Backend.S) = !Backend.main in
  M.reset ()

let reset_flush_history () =
  let (module M : Backend.S) = !Backend.main in
  M.reset_flush_history ()

let finish () =
  let (module M : Backend.S) = !Backend.main in
  M.finish ()

module Status_line = struct
  type t =
    | Live of (unit -> User_message.Style.t Pp.t)
    | Constant of User_message.Style.t Pp.t

  module Id = Id.Make ()

  let toplevel = Id.gen ()

  let stack = ref []

  let refresh () =
    match !stack with
    | [] -> set_status_line None
    | (_id, t) :: _ ->
      let pp =
        match t with
        | Live f -> f ()
        | Constant x -> x
      in
      (* Always put the status line inside a horizontal box to force the
         [Format] module to prefer a single line. In particular, it seems that
         [Format.pp_print_text] split the line before the last word, unless it
         is succeeded by a space. This seems like a bug in [Format] and putting
         the whole thing into a [hbox] works around this bug.

         See https://github.com/ocaml/dune/issues/2779 *)
      set_status_line (Some (Pp.hbox pp))

  let set t =
    stack := [ (toplevel, t) ];
    (match t with
    | Live _ -> ()
    | Constant pp -> print_if_no_status_line pp);
    refresh ()

  let clear () =
    stack := [];
    refresh ()

  type overlay = Id.t

  let add_overlay t =
    let id = Id.gen () in
    stack := (id, t) :: !stack;
    refresh ();
    id

  let remove_overlay id =
    stack := List.filter !stack ~f:(fun (id', _) -> not (Id.equal id id'));
    refresh ()

  let with_overlay t ~f =
    let id = add_overlay t in
    Exn.protect ~f ~finally:(fun () -> remove_overlay id)
end

let () = User_warning.set_reporter print_user_message
