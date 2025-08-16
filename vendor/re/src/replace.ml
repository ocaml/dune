let replace ?(pos = 0) ?len ?(all = true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit =
    match len with
    | None -> String.length s
    | Some l ->
      if l < 0 || pos + l > String.length s then invalid_arg "Re.replace";
      pos + l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos on_match =
    if pos <= limit
    then (
      match
        Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
      with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        if pos = p1 && p1 = p2 && on_match
        then (
          (* if we matched an empty string right after a match,
             we must manually advance by 1 *)
          if p2 < limit then Buffer.add_char buf s.[p2];
          iter (p2 + 1) false)
        else (
          (* add string between previous match and current match *)
          Buffer.add_substring buf s pos (p1 - pos);
          (* what should we replace the matched group with? *)
          let replacing = f substr in
          Buffer.add_string buf replacing;
          if all
          then
            (* if we matched an empty string, we must manually advance by 1 *)
            iter
              (if p1 = p2
               then (
                 (* a non char could be past the end of string. e.g. $ *)
                 if p2 < limit then Buffer.add_char buf s.[p2];
                 p2 + 1)
               else p2)
              (p1 <> p2)
          else Buffer.add_substring buf s p2 (limit - p2))
      | Running _ -> ()
      | Failed -> Buffer.add_substring buf s pos (limit - pos))
  in
  iter pos false;
  Buffer.contents buf
;;

let replace_string ?pos ?len ?all re ~by s = replace ?pos ?len ?all re s ~f:(fun _ -> by)
