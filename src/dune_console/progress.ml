open Stdune

module No_flush = struct
  let status_line = ref Pp.nop
  let start () = ()
  let status_line_height = ref 0
  let ansi_erase = "\r\027[2K"
  let ansi_up = "\027M"
  let clear_code_length = String.length ansi_erase + String.length ansi_up

  let hide_status_line () =
    if !status_line_height > 0
    then (
      let buf = Buffer.create (clear_code_length * !status_line_height) in
      Buffer.add_string buf ansi_erase;
      for _ = 1 to !status_line_height do
        Buffer.add_string buf (ansi_up ^ ansi_erase)
      done;
      Buffer.contents buf |> Printf.eprintf "%s")
  ;;

  let show_status_line () = if !status_line_height > 0 then Ansi_color.prerr !status_line

  let set_status_line = function
    | None ->
      hide_status_line ();
      status_line := Pp.nop;
      status_line_height := 0
    | Some line ->
      let line = Pp.map_tags line ~f:User_message.Print_config.default in
      hide_status_line ();
      status_line := line;
      status_line_height
        := Format.asprintf "%a" Pp.to_fmt line |> String.split_lines |> List.length;
      show_status_line ()
  ;;

  let print_if_no_status_line _msg = ()

  let print_user_message msg =
    hide_status_line ();
    Dumb.No_flush.print_user_message msg;
    show_status_line ()
  ;;

  let reset () = Dumb.reset ()
  let finish () = set_status_line None
  let reset_flush_history () = Dumb.reset_flush_history ()
end

let no_flush = (module No_flush : Backend_intf.S)
let flush = Combinators.flush no_flush
