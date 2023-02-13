open Stdune

module Backend = struct
  type t = Backend_intf.t

  module Dumb_no_flush : Backend_intf.S = struct
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

  module Dumb : Backend_intf.S = struct
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

  module Progress_no_flush : Backend_intf.S = struct
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

  let dumb = (module Dumb : Backend_intf.S)

  let progress = (module Progress_no_flush : Backend_intf.S)

  let main = ref dumb

  let set (module T : Backend_intf.S) =
    let module Old = (val !main) in
    Old.finish ();
    main := (module T);
    T.start ()

  let compose (module A : Backend_intf.S) (module B : Backend_intf.S) :
      (module Backend_intf.S) =
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
    end : Backend_intf.S)

  module Progress_no_flush_threaded : Threaded_intf.S = struct
    include Progress_no_flush

    let render (state : Threaded_intf.state) =
      while not (Queue.is_empty state.messages) do
        print_user_message (Queue.pop_exn state.messages)
      done;
      set_status_line state.status_line;
      flush stderr

    (* The current console doesn't react to user events so we just sleep until
       the next loop iteration. Because it doesn't react to user input, it cannot
       modify the UI state, and as a consequence doesn't need the mutex. *)
    let handle_user_events ~now ~time_budget _ =
      Unix.sleepf time_budget;
      now +. time_budget
  end

  let progress_threaded =
    let t = lazy (Threaded.make (module Progress_no_flush_threaded)) in
    fun () -> Lazy.force t
end

module Threaded = struct
  include Threaded_intf
  include Threaded
end

let print_user_message msg =
  let (module M : Backend_intf.S) = !Backend.main in
  M.print_user_message msg

let print paragraphs = print_user_message (User_message.make paragraphs)

let printf fmt = Printf.ksprintf (fun msg -> print [ Pp.verbatim msg ]) fmt

let set_status_line line =
  let (module M : Backend_intf.S) = !Backend.main in
  M.set_status_line line

let print_if_no_status_line line =
  let (module M : Backend_intf.S) = !Backend.main in
  M.print_if_no_status_line line

let reset () =
  let (module M : Backend_intf.S) = !Backend.main in
  M.reset ()

let reset_flush_history () =
  let (module M : Backend_intf.S) = !Backend.main in
  M.reset_flush_history ()

let finish () =
  let (module M : Backend_intf.S) = !Backend.main in
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
