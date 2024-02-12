open Import
open Lwd.O
module Unicode = Drawing.Unicode

let create () = Term.create ~nosig:false ~output:Unix.stderr ()
let bytes = Bytes.make 64 '0'
let sigcont_pipe = lazy (Unix.pipe ~cloexec:true ())

let term =
  let setup =
    lazy
      (Unix.set_nonblock (Lazy.force sigcont_pipe |> fst);
       let term = ref (create ()) in
       Sys.set_signal Sys.sigcont
       @@ Sys.Signal_handle
            (fun _ ->
              Term.release !term;
              term := create ();
              assert (1 = Unix.single_write (Lazy.force sigcont_pipe |> snd) bytes 0 1));
       let rec old =
         lazy
           (Sys.signal Sys.sigtstp
            @@ Sys.Signal_handle
                 (fun i ->
                   Term.release !term;
                   match Lazy.force old with
                   | Sys.Signal_handle f -> f i
                   | _ -> Unix.kill (Unix.getpid ()) Sys.sigstop))
       in
       ignore (Lazy.force old);
       term)
  in
  fun () -> !(Lazy.force setup)
;;

(* style for diving visual elements like borders or rules *)
let divider_attr = A.(fg red)

(* style for helpful ui elements like scrollbar structures or help text *)
let helper_attr = A.(fg yellow)

(* style for user feedback like message count, or scrollbar position *)
let user_feedback_attr = A.(fg cyan)

(* Here we keep some persistent state about the program that we update each time we
   render. This allows other components to "react" to changes using [Lwd]. *)

let term_size = Lwd.var (0, 0)
let messages = Lwd.var []
let status_line = Lwd.var None

module Message_viewer = struct
  (* Specialized widget for viewing messages in a list. *)

  (* [is_message_hidden] tracks if a given message is hidden. [true] means it is hidden
     and [false] means it is expanded. *)
  let is_message_hidden = Lwd.var (fun _ -> false)

  let message_images =
    let+ messages = Lwd.get messages in
    List.map messages ~f:(fun x -> Drawing.pp_to_image (User_message.pp x))
  ;;

  (* We calculate the max message length from all the messages. This is used to keep the
     horizontal lines with a consistent length. *)
  let max_message_length =
    let+ messages = message_images
    and+ w, _ = Lwd.get term_size in
    let lengths = List.map messages ~f:I.width in
    match List.max lengths ~f:Int.compare with
    | None -> w - 1
    | Some l -> max l w
  ;;

  (* We approximate the first line of the message. Unfortunately due to the way Notty
     images work, it is not easy to get the actual width of the first line. Therefore we
     just chop a third off as an approximation. *)
  let message_synopsis ~attr =
    let+ messages = message_images
    and+ width, _ = Lwd.get term_size in
    fun index ->
      match List.nth messages index with
      | None -> I.string attr "..."
      | Some message ->
        let cropped_image = I.vcrop 0 (I.height message - 1) message in
        I.hcrop 0 (min (I.width cropped_image / 3) (width - 15)) cropped_image
  ;;

  (* This is a line that shows the total number of messages and the message count used
     for separating messages. It also has a handler for clicks that minimizes the
     message. *)
  let horizontal_line_with_count total index =
    let+ is_hidden = Lwd.get is_message_hidden
    and+ w = max_message_length
    and+ synopsis = Lwd.app (message_synopsis ~attr:helper_attr) (Lwd.return index) in
    let index_is_hidden = is_hidden index in
    let status =
      I.hcat
        [ I.uchar divider_attr Unicode.ogham_reversed_feather_mark 1 1
        ; I.string user_feedback_attr (string_of_int (index + 1))
        ; I.string divider_attr "/"
        ; I.string user_feedback_attr (string_of_int total)
        ; I.uchar divider_attr Unicode.ogham_feather_mark 1 1
        ]
    in
    let toggle_indicator =
      I.hcat
        [ I.uchar divider_attr Unicode.ogham_reversed_feather_mark 1 1
        ; (if index_is_hidden then I.string helper_attr "+" else I.string helper_attr "-")
        ; I.uchar divider_attr Unicode.ogham_feather_mark 1 1
        ]
    in
    let synopsis =
      if index_is_hidden
      then
        I.hcat
          [ I.hpad 1 0 @@ I.uchar divider_attr Unicode.ogham_reversed_feather_mark 1 1
          ; synopsis
          ; I.uchar divider_attr Unicode.ogham_feather_mark 1 1
          ]
      else I.empty
    in
    let mouse_handler ~x:_ ~y = function
      | `Left ->
        if y = 0
        then (
          Lwd.set is_message_hidden (fun x ->
            if x = index then not index_is_hidden else is_hidden x);
          `Handled)
        else `Unhandled
      | _ -> `Unhandled
    in
    Ui.atom
    @@ I.zcat
         I.
           [ I.hsnap ~align:`Left w (toggle_indicator <|> status <|> synopsis)
           ; Drawing.horizontal_rule ~w ~attr:divider_attr
           ]
    |> Ui.mouse_area mouse_handler
  ;;

  (* This components displays a line followed by a message. Cliking on the line will
     collapse the message. *)
  let line_separated_message ~total index msg =
    let+ horizontal_line_with_count = horizontal_line_with_count total index
    and+ toggle = Lwd.app (Lwd.get is_message_hidden) (Lwd.return index) in
    if toggle
    then horizontal_line_with_count
    else Ui.vcat [ horizontal_line_with_count; Ui.atom msg ]
  ;;

  (* We take all the messages in the console and display them as
     [line_separated_message]s. We also handle arrow keys/vim bindings together with mouse
     scrolling for the scrolling effect. We handle ['m'] as an expand / collapse all
     messages function. *)
  let ui =
    let expand_all = Lwd.var false in
    let scrollbox_state = Lwd.var Scrollbox.State.init in
    let* messages = message_images
    and+ w = max_message_length in
    let+ { ui; vscroll; hscroll } =
      let image =
        let+ messages =
          List.mapi messages ~f:(line_separated_message ~total:(List.length messages))
          |> Lwd_utils.flatten_l
        in
        match messages with
        | [] -> Ui.empty
        | messages ->
          Ui.vcat (messages @ [ Ui.atom @@ Drawing.horizontal_rule ~w ~attr:helper_attr ])
      in
      Scrollbox.make scrollbox_state @@ image
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
      (* Toggle expand all *)
      | `ASCII 'm', _ ->
        Lwd.set expand_all (not (Lwd.peek expand_all));
        Lwd.set is_message_hidden (fun _ -> Lwd.peek expand_all);
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
  ;;
end

(* Help pop up contains help information for the user. It also includes a hook so that
   other compoenents can trigger the help screen. *)
let help_box =
  let help_screen_lines =
    [ "Press 'q' to quit"
    ; "Press '?' to toggle this screen"
    ; "Navigate with the mouse or arrow keys (or vim bindings)"
    ; "Press 'm' to expand / collapse all messages"
    ]
  in
  let* width, height = Lwd.get term_size in
  Help_box.make ~helper_attr ~divider_attr ~help_screen_lines ~width ~height
;;

(* The status bar shows the build status and includes a help button. *)
let status_bar =
  let+ w, _ = Lwd.get term_size
  and+ help_button =
    let+ { toggle; _ } = help_box in
    let image =
      I.hcat
        [ I.uchar helper_attr Unicode.ogham_reversed_feather_mark 1 1
        ; I.char user_feedback_attr '?' 1 1
        ; I.uchar helper_attr Unicode.ogham_feather_mark 1 1
        ]
    in
    Button.of_ (Ui.atom image) toggle
  and+ status =
    Lwd.get status_line
    >>| function
    | None -> I.empty
    | Some message -> Drawing.pp_to_image message
  in
  let status =
    I.hcat
      [ I.uchar helper_attr Unicode.ogham_reversed_feather_mark 1 1
      ; I.char helper_attr ' ' 1 1
      ; status
      ; I.char helper_attr ' ' 1 1
      ; I.uchar helper_attr Unicode.ogham_feather_mark 1 1
      ]
  in
  let hsnap_or_leave ~width img =
    if I.width img < width then I.hsnap ~align:`Middle width img else img
  in
  Ui.zcat
    [ Ui.atom
      @@ I.zcat
           [ hsnap_or_leave ~width:w status
           ; Drawing.horizontal_rule ~attr:helper_attr ~w
           ]
    ; help_button
    ]
;;

(* Our document has 3 components:
   - A help box
   - A status bar
   - A message viewer *)
let document =
  let* { ui = help_box; toggle = handle_help } = help_box in
  let+ status_bar = status_bar
  and+ message_viewer = Message_viewer.ui in
  let keyboard_handler = function
    (* When we encounter q we make sure to quit by signaling termination. *)
    | `ASCII 'q', _ ->
      Unix.kill (Unix.getpid ()) Sys.sigterm;
      `Handled
    (* Toggle help screen *)
    | `ASCII '?', _ ->
      handle_help ();
      `Handled
    | _ -> `Unhandled
  in
  Ui.zcat [ Ui.vcat [ status_bar; message_viewer ]; help_box ]
  |> Ui.keyboard_area keyboard_handler
;;

module Console_backend = struct
  let start () = ()
  let reset () = ()
  let renderer = Renderer.make ()

  let set_state =
    let update equal v x = if not (equal (Lwd.peek v) x) then Lwd.set v x in
    fun (state : Dune_threaded_console.state) ->
      let size = Term.size (term ()) in
      update (Tuple.T2.equal Int.equal Int.equal) term_size size;
      update
        (Option.equal (fun x y ->
           Ordering.is_eq (Pp.compare ~compare:User_message.Style.compare x y)))
        status_line
        state.status_line;
      if let l = Lwd.peek messages in
         not
           (List.length l = Queue.length state.messages
            && List.equal User_message.equal l (Queue.to_list state.messages))
      then Lwd.set messages (Queue.to_list state.messages)
  ;;

  let render (state : Dune_threaded_console.state) =
    let size = Term.size (term ()) in
    (* Update the persistent values tracked by other components. *)
    set_state state;
    (* This is a standard [Lwd] routine for creating a document. *)
    let root = Lwd.observe document in
    let image =
      let rec stabilize () =
        let tree = Lwd.quick_sample root in
        Renderer.update renderer size tree;
        let image = Renderer.image renderer in
        if Lwd.is_damaged root then stabilize () else image
      in
      stabilize ()
    in
    (* Finally we use Notty to show the image. *)
    Term.image (term ()) image
  ;;

  (* Update any global state and finish *)
  let set_dirty ~mutex (state : Dune_threaded_console.state) =
    Mutex.lock mutex;
    state.dirty <- true;
    Mutex.unlock mutex;
    Unix.gettimeofday ()
  ;;

  let rec handle_user_events ~now ~time_budget mutex (state : Dune_threaded_console.state)
    =
    (* We check for any user input and handle it. If we go over the [time_budget] we give
       up and continue. *)
    let input_fds =
      let sigcont_r, _ = Lazy.force sigcont_pipe in
      match Unix.select [ Unix.stdin; sigcont_r ] [] [] time_budget with
      | exception Unix.Unix_error (EINTR, _, _) -> `Restore
      | [], _, _ -> `Timeout
      | fds, _, _ ->
        (match List.exists fds ~f:(Poly.equal sigcont_r) with
         | false -> `Event
         | true ->
           (match Unix.read sigcont_r bytes 0 1 with
            | exception Unix.Unix_error (EBADF, _, _) -> assert false
            | (exception Unix.Unix_error _) | _ -> ());
           (* backgrounding could have changed the cursor settings for example. we need to
              restore all this stuff *)
           `Restore)
    in
    match input_fds with
    | `Restore -> set_dirty ~mutex state
    | `Timeout ->
      now +. time_budget
      (* Nothing to do, we return the time at the end of the time budget. *)
    | `Event ->
      (match Term.event (term ()) with
       | `End -> set_dirty ~mutex state
       (* on resize we wish to redraw so the state is set to dirty *)
       | `Resize (_width, _height) -> set_dirty ~mutex state
       | #Notty.Unescape.event as event ->
         let event = (event : Notty.Unescape.event :> Ui.event) in
         ignore (Renderer.dispatch_event renderer event : [ `Handled | `Unhandled ]);
         set_dirty ~mutex state
       | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
         (* If we encounter an exception, we make sure to rehandle user events with a
            corrected time budget. *)
         let old_now = now in
         let now = Unix.gettimeofday () in
         let delta_now = now -. old_now in
         let time_budget = Float.max 0. (time_budget -. delta_now) in
         handle_user_events ~now ~time_budget mutex state)
  ;;

  let reset_flush_history () = ()
  let finish () = Term.release (term ())
end

let backend =
  let t =
    lazy
      (Dune_threaded_console.make
         ~frames_per_second:(Dune_util.frames_per_second ())
         (module Console_backend))
  in
  fun () ->
    match (Platform.OS.value : Platform.OS.t) with
    | Windows -> User_error.raise [ Pp.text "TUI is currently not supported on Windows." ]
    | Linux | Darwin | FreeBSD | OpenBSD | NetBSD | Haiku | Other -> Lazy.force t
;;
