open Stdune

module Backend = struct
  module type S = sig
    val print_user_message : User_message.t -> unit

    val set_status_line : User_message.Style.t Pp.t option -> unit

    val print_if_no_status_line : User_message.Style.t Pp.t -> unit

    val reset : unit -> unit
  end

  type t = (module S)

  module Dumb_no_flush : S = struct
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
  end

  module Progress : S = struct
    let status_line = ref Pp.nop

    let status_line_len = ref 0

    let hide_status_line () =
      if !status_line_len > 0 then Printf.eprintf "\r%*s\r" !status_line_len ""

    let show_status_line () =
      if !status_line_len > 0 then Ansi_color.prerr !status_line

    let set_status_line = function
      | None ->
        hide_status_line ();
        status_line := Pp.nop;
        status_line_len := 0;
        flush stderr
      | Some line ->
        let line = Pp.map_tags line ~f:User_message.Print_config.default in
        let line_len = String.length (Format.asprintf "%a" Pp.to_fmt line) in
        hide_status_line ();
        status_line := line;
        status_line_len := line_len;
        show_status_line ();
        flush stderr

    let print_if_no_status_line _msg = ()

    let print_user_message msg =
      hide_status_line ();
      Dumb_no_flush.print_user_message msg;
      show_status_line ();
      flush stderr

    let reset () = Dumb.reset ()
  end

  let dumb = (module Dumb : S)

  let progress = (module Progress : S)

  let main = ref dumb

  let set t = main := t

  let compose (module A : S) (module B : S) : (module S) =
    (module struct
      let print_user_message msg =
        A.print_user_message msg;
        B.print_user_message msg

      let set_status_line x =
        A.set_status_line x;
        B.set_status_line x

      let print_if_no_status_line msg =
        A.print_if_no_status_line msg;
        B.print_if_no_status_line msg

      let reset () =
        A.reset ();
        B.reset ()
    end : S)
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
