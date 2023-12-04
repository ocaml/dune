open Stdune

module type Backend = Backend_intf.S

module Backend = struct
  type t = Backend_intf.t

  let dumb = (module Dumb : Backend_intf.S)
  let progress = Progress.flush
  let compose = Combinators.compose
  let main = ref dumb

  let set (module T : Backend_intf.S) =
    let module Old = (val !main) in
    Old.finish ();
    main := (module T);
    T.start ()
  ;;

  let flush t = Combinators.flush t
  let progress_no_flush = Progress.no_flush
end

(* Flag that controls whether messages should be separated by a blank line *)
let separate_messages_flag = ref false

(* A user message that solely contains a blank line *)
let blank_line_msg =
  { User_message.paragraphs = [ Pp.cut ]
  ; hints = []
  ; annots = User_message.Annots.empty
  ; loc = None
  }
;;

(** Prints a blank line *)
let print_blank_line () =
  let (module M : Backend_intf.S) = !Backend.main in
  M.print_user_message blank_line_msg
;;

let first_msg = ref true
let separate_messages v = separate_messages_flag := v

(* If the [separate_messages = false], then [print_blank_line ()] does nothing.
   When [separate_messages = true], [print_blank_line ()] does nothing the
   first time it is called, whereas subsequent calls print a new line. Note
   that calls to [reset] or [reset_flush_history] will erase the information
   of whether some message has already been printed. As a consequence, after a
   call to [reset] or [reset_flush_history], [print_blank_line] will behave as
   if it has never been called before. *)
let print_blank_line () =
  if !separate_messages_flag
  then
    (* only do something when the flag is on, i.e. the first time
       the function is called *)
    if !first_msg
    then
      (* do not print anything the first time the function is
         called, but remember it has been called at least once *)
      first_msg := false
    else
      (* if the function has already been called at least once,
         print a blank line *)
      print_blank_line ()
;;

let print_user_message msg =
  let (module M : Backend_intf.S) = !Backend.main in
  print_blank_line ();
  M.print_user_message msg
;;

let print paragraphs = print_user_message (User_message.make paragraphs)
let printf fmt = Printf.ksprintf (fun msg -> print [ Pp.verbatim msg ]) fmt

let set_status_line line =
  let (module M : Backend_intf.S) = !Backend.main in
  M.set_status_line line
;;

let print_if_no_status_line line =
  let (module M : Backend_intf.S) = !Backend.main in
  M.print_if_no_status_line line
;;

let reset () =
  (* forget that [print_user_message] has ever been called *)
  first_msg := true;
  let (module M : Backend_intf.S) = !Backend.main in
  M.reset ()
;;

let reset_flush_history () =
  (* forget that [print_user_message] has ever been called *)
  first_msg := true;
  let (module M : Backend_intf.S) = !Backend.main in
  M.reset_flush_history ()
;;

let finish () =
  let (module M : Backend_intf.S) = !Backend.main in
  M.finish ()
;;

let () = at_exit finish

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
  ;;

  let set t =
    stack := [ toplevel, t ];
    (match t with
     | Live _ -> ()
     | Constant pp -> print_if_no_status_line pp);
    refresh ()
  ;;

  let clear () =
    stack := [];
    refresh ()
  ;;

  type overlay = Id.t

  let add_overlay t =
    let id = Id.gen () in
    stack := (id, t) :: !stack;
    refresh ();
    id
  ;;

  let remove_overlay id =
    stack := List.filter !stack ~f:(fun (id', _) -> not (Id.equal id id'));
    refresh ()
  ;;

  let with_overlay t ~f =
    let id = add_overlay t in
    Exn.protect ~f ~finally:(fun () -> remove_overlay id)
  ;;
end

let () = User_warning.set_reporter print_user_message
