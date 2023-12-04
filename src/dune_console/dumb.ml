open Stdune

module No_flush : Backend_intf.S = struct
  let start () = ()
  let finish () = ()

  let print_user_message msg =
    Option.iter msg.User_message.loc ~f:(fun loc ->
      Loc.render Format.err_formatter (Loc.pp loc));
    User_message.prerr { msg with loc = None }
  ;;

  let set_status_line _ = ()

  let print_if_no_status_line msg =
    (* [Pp.cut] seems to be enough to force the terminating newline to
       appear. *)
    Ansi_color.prerr
      (Pp.seq (Pp.map_tags msg ~f:User_message.Print_config.default) Pp.cut)
  ;;

  let reset () = prerr_string "\x1b[H\x1b[2J"
  let reset_flush_history () = prerr_string "\x1b[1;1H\x1b[2J\x1b[3J"
end

let flush = Combinators.flush (module No_flush)

include (val flush)
