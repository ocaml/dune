(* Small helper to find out who is the caller of a function *)

let get ~skip =
  let skip = __FILE__ :: skip in
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
  let rec loop pos =
    if pos = len
    then None
    else (
      match
        Printexc.get_raw_backtrace_slot stack pos
        |> Printexc.convert_raw_backtrace_slot
        |> Printexc.Slot.location
      with
      | None -> None
      | Some loc ->
        if List.mem skip loc.filename ~equal:String.equal
        then loop (pos + 1)
        else (
          let start : Lexing.position =
            { pos_fname = loc.filename
            ; pos_lnum = loc.line_number
            ; pos_bol = 0
            ; pos_cnum = loc.start_char
            }
          in
          let stop = { start with pos_cnum = loc.end_char } in
          Some (Loc.create ~start ~stop)))
  in
  loop 0
;;
