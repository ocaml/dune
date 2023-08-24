open Stdune

module No_flush = struct
  let status = ref []
  let start () = ()

  let hide_status =
    (* This ansi code will clear the entire line leaving the cursor in place. *)
    let ansi_erase = "\027[2K" in
    (* Move the cursor up one line. *)
    let ansi_up = "\027M" in
    fun () ->
      let status_height = List.length !status in
      if status_height > 0
      then (
        (* Return the cursor to the begining of the line and clear it. *)
        Printf.eprintf "\r%s" ansi_erase;
        for _ = 1 to status_height - 1 do
          (* If we have multiple lines, we move the cursor up and clear. *)
          Printf.eprintf "%s%s" ansi_up ansi_erase
        done)
  ;;

  let show_status () =
    if List.length !status > 0
    then (
      let width =
        match Notty_unix.winsize Unix.stderr with
        | Some (width, _) -> width
        | None -> Dune_config.Config.(get console_width_fallback)
      in
      List.map ~f:(fun line -> Pp.truncate width line |> Pp.hbox) !status
      |> Pp.concat ~sep:Pp.newline
      |> Ansi_color.prerr)
  ;;

  let set_status = function
    | [] ->
      hide_status ();
      status := []
    | lines ->
      let lines = List.map ~f:(Pp.map_tags ~f:User_message.Print_config.default) lines in
      hide_status ();
      status := lines;
      show_status ()
  ;;

  let print_if_no_status _msg = ()

  let print_user_message msg =
    hide_status ();
    Dumb.No_flush.print_user_message msg;
    show_status ()
  ;;

  let reset () = Dumb.reset ()
  let finish () = set_status []
  let reset_flush_history () = Dumb.reset_flush_history ()
end

let no_flush = (module No_flush : Backend_intf.S)
let flush = Combinators.flush no_flush
