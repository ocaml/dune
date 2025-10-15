open Import
open Lwd.O
module Unicode = Drawing.Unicode

let bytes = Bytes.make 64 '0'
let sigcont_pipe = lazy (Unix.pipe ~cloexec:true ())

module Terminal_manager = struct
  let term_ref : Notty_unix.Term.t option ref = ref None
  let old_sigtstp_handler = ref Sys.Signal_default

  let get_term () =
    match !term_ref with
    | Some term -> term
    | None -> Code_error.raise "Terminal is not enabled." []
  ;;

  let disable () =
    match !term_ref with
    | None -> ()
    | Some term ->
      Term.release term;
      term_ref := None;
      Sys.set_signal Sys.sigtstp !old_sigtstp_handler;
      Sys.set_signal Sys.sigcont Sys.Signal_default
  ;;

  let rec enable () =
    match !term_ref with
    | Some _ -> ()
    | None ->
      let term = Term.create ~nosig:false ~output:Unix.stderr () in
      term_ref := Some term;
      (* Install signal handlers for user-initiated suspend/resume.
         The SIGCONT handler should re-enable the terminal. *)
      let sigcont_pipe_r, sigcont_pipe_w = Lazy.force sigcont_pipe in
      Unix.set_nonblock sigcont_pipe_r;
      old_sigtstp_handler
      := Sys.signal Sys.sigtstp
         @@ Sys.Signal_handle
              (fun i ->
                disable ();
                match !old_sigtstp_handler with
                | Sys.Signal_handle f -> f i
                | _ -> Unix.kill (Unix.getpid ()) Sys.sigstop);
      Sys.set_signal Sys.sigcont
      @@ Sys.Signal_handle
           (fun _ ->
             if not (Option.is_some !term_ref)
             then (
               let _ = enable () in
               assert (1 = Unix.single_write sigcont_pipe_w bytes 0 1)))
  ;;
end

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
    let+ messages = Lwd.get messages
    and+ w, _ = Lwd.get term_size in
    List.map messages ~f:(fun x -> Drawing.pp_to_image ~w (User_message.pp x))
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

  (* Stolen from RPC *)
  let make_loc { Ocamlc_loc.path; chars; lines } : Loc.t =
    let pos_lnum_start, pos_lnum_stop =
      match lines with
      | Single i -> i, i
      | Range (i, j) -> i, j
    in
    let pos_cnum_start, pos_cnum_stop =
      match chars with
      | None -> 0, 0
      | Some (x, y) -> x, y
    in
    let pos = { Lexing.pos_fname = path; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } in
    let start = { pos with pos_lnum = pos_lnum_start; pos_cnum = pos_cnum_start } in
    let stop = { pos with pos_lnum = pos_lnum_stop; pos_cnum = pos_cnum_stop } in
    Stdune.Loc.create ~start ~stop
  ;;

  let message_filename =
    let+ messages = Lwd.get messages in
    fun index ->
      let open Option.O in
      let* message = List.nth messages index in
      let loc = message.loc |> Option.value ~default:Loc.none in
      if Loc.is_none loc
      then
        if User_message.has_embedded_location message
        then (* try to extract location *)
          User_message.pp message
          |> Format.asprintf "%a" Pp.to_fmt
          |> Ocamlc_loc.parse_raw
          (* We only go to the first location, because we don't have a good way
             of separating the locations in the message later. *)
          |> List.find_map ~f:(function
            | `Loc loc -> Some (make_loc loc)
            | _ -> None)
        else None
      else Some loc
  ;;

  (* Here we crop the message horizontally so that the first line will fit in
     the synopsis. The only issue here is that we don't know the real width of
     the first line but only the width of the message as a whole. This means if
     the second line is longer than the first, there will be some padding in
     the synopsis. *)
  let message_synopsis ~attr =
    let+ messages = message_images
    and+ t_width, _ = Lwd.get term_size in
    fun index ->
      match List.nth messages index with
      | None -> I.string attr "..."
      | Some message ->
        let cropped_image = I.vcrop 0 (I.height message - 1) message in
        let max_width = t_width - 12 in
        let img_width = I.width cropped_image in
        if img_width > max_width
        then (
          let ellipsis = I.string attr "..." in
          let trimmed =
            I.hcrop 0 (img_width - max 0 (max_width - I.width ellipsis)) cropped_image
          in
          I.(trimmed <|> ellipsis </> I.char A.empty ' ' max_width 1))
        else I.(cropped_image </> I.char A.empty ' ' img_width 1)
  ;;

  let open_editor loc =
    match
      let open Option.O in
      let* loc = loc in
      let env = Env.initial in
      let* editor = Env.get env "EDITOR" in
      let+ prog = Bin.which ~path:(Env_path.path env) editor >>| Path.to_string in
      let argv =
        let { Lexing.pos_fname = file_to_open; pos_lnum = line; pos_cnum = col; _ } =
          Loc.start loc
        in
        (* Editor specific logic for opening files at a location. *)
        (* TODO: should we pass [prog] or [editor] to argv.(0)? *)
        match editor with
        | "vi" | "vim" | "nvim" ->
          let position_arg = String.concat ~sep:"" [ "+"; Int.to_string line ] in
          [ prog; position_arg; file_to_open ]
        | "emacs" ->
          let position_arg =
            String.concat ~sep:"" [ "+"; Int.to_string line; ":"; Int.to_string col ]
          in
          [ prog; position_arg; file_to_open ]
        | "code" ->
          let position_arg =
            String.concat ~sep:":" [ file_to_open; Int.to_string line; Int.to_string col ]
          in
          [ prog; "-g"; position_arg ]
        (* TODO: subl, nano, ... ? *)
        | _ -> [ prog; file_to_open ]
      in
      prog, argv
    with
    | Some (prog, argv) ->
      Terminal_manager.disable ();
      let () =
        let _pid, exit_status =
          Dune_spawn.Spawn.spawn ~prog ~argv () |> Unix.waitpid []
        in
        Dune_util.Log.command
          ~command_line:(String.concat ~sep:" " argv)
          ~output:""
          ~exit_status
      in
      Terminal_manager.enable ();
      `Handled
    | None -> `Handled
  ;;

  (* This is a line that shows the total number of messages and the message count used
     for separating messages. It also has a handler for clicks that minimizes the
     message. *)
  let horizontal_line_with_count total index =
    let+ is_hidden = Lwd.get is_message_hidden
    and+ w = max_message_length
    and+ synopsis = Lwd.app (message_synopsis ~attr:helper_attr) (Lwd.return index)
    and+ file_to_open = Lwd.app message_filename (Lwd.return index) in
    let index_is_hidden = is_hidden index in
    let status =
      I.hcat
        [ I.uchar divider_attr (Uchar.of_char '[') 1 1
        ; I.string user_feedback_attr (string_of_int (index + 1))
        ; I.string divider_attr "/"
        ; I.string user_feedback_attr (string_of_int total)
        ; I.uchar divider_attr (Uchar.of_char ']') 1 1
        ]
    in
    let toggle_indicator =
      I.hcat
        [ I.uchar divider_attr (Uchar.of_char '[') 1 1
        ; (if index_is_hidden then I.string helper_attr "+" else I.string helper_attr "-")
        ; I.uchar divider_attr (Uchar.of_char ']') 1 1
        ]
    in
    let synopsis =
      if index_is_hidden
      then
        I.hcat
          [ I.hpad 1 0 @@ I.uchar divider_attr (Uchar.of_char '[') 1 1
          ; synopsis
          ; I.uchar divider_attr (Uchar.of_char ']') 1 1
          ]
      else I.empty
    in
    let toggle_minimize () =
      Lwd.set is_message_hidden (fun x ->
        if x = index then not index_is_hidden else is_hidden x);
      `Handled
    in
    let mouse_handler ~x ~y = function
      | `Left when 0 <= x && x < 3 && y = 0 -> toggle_minimize ()
      | `Left when 3 <= x && y = 0 -> open_editor file_to_open
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
    and+ toggle = Lwd.app (Lwd.get is_message_hidden) (Lwd.return index)
    and+ file_to_open = Lwd.app message_filename (Lwd.return index) in
    if toggle
    then horizontal_line_with_count
    else (
      let mouse_handler ~x:_ ~y:_ = function
        | `Left -> open_editor file_to_open
        | _ -> `Unhandled
      in
      Ui.vcat [ horizontal_line_with_count; Ui.atom msg |> Ui.mouse_area mouse_handler ])
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
    ; "Left click on messages to open EDITOR at the specified location."
    ]
  in
  let* width, height = Lwd.get term_size in
  Help_box.make ~helper_attr ~divider_attr ~help_screen_lines ~width ~height
;;

(* The status bar shows the build status and includes a help button. *)
let status_bar =
  let* w, _ = Lwd.get term_size in
  let+ help_button =
    let+ { toggle; _ } = help_box in
    let image =
      I.hcat
        [ I.uchar helper_attr (Uchar.of_char '[') 1 1
        ; I.char user_feedback_attr '?' 1 1
        ; I.uchar helper_attr (Uchar.of_char ']') 1 1
        ]
    in
    Button.of_ (Ui.atom image) toggle
  and+ status =
    Lwd.get status_line
    >>| function
    | None -> I.empty
    | Some message -> Drawing.pp_to_image ~w message
  in
  let status =
    I.hcat [ I.char helper_attr ' ' 1 1; status; I.char helper_attr ' ' 1 1 ]
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
  let start () = Terminal_manager.enable ()
  let reset () = ()
  let renderer = Renderer.make ()

  let set_state =
    let update equal v x = if not (equal (Lwd.peek v) x) then Lwd.set v x in
    fun (state : Dune_threaded_console.state) ->
      let size = Term.size (Terminal_manager.get_term ()) in
      update (Tuple.T2.equal Int.equal Int.equal) term_size size;
      update
        (Option.equal (fun x y ->
           Ordering.is_eq (Pp.compare ~compare:User_message.Style.compare x y)))
        status_line
        state.status_line;
      if
        let l = Lwd.peek messages in
        not
          (List.length l = Queue.length state.messages
           && List.equal User_message.equal l (Queue.to_list state.messages))
      then Lwd.set messages (Queue.to_list state.messages)
  ;;

  let render (state : Dune_threaded_console.state) =
    let size = Term.size (Terminal_manager.get_term ()) in
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
    Term.image (Terminal_manager.get_term ()) image
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
      (match Term.event (Terminal_manager.get_term ()) with
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
  let finish () = Terminal_manager.disable ()
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
