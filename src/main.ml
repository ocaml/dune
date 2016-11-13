open Import

let internal argv =
  match Array.to_list argv with
  | [_; "findlib-packages"] ->
    List.iter (Findlib.all_packages ()) ~f:(Printf.printf "%s\n")
  | _ ->
    ()

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  let compact () =
    Array.append
      [|sprintf "%s %s" argv.(0) argv.(1)|]
      (Array.sub argv ~pos:2 ~len:(argc - 2))
  in
  if argc >= 2 then
    match argv.(1) with
    | "internal" -> internal (compact ())
    | _ -> ()

let () =
  try
    main ()
  with
  | Loc.Error ({ start; stop }, msg) ->
    let start_c = start.pos_cnum - start.pos_bol in
    let stop_c  = stop.pos_cnum  - start.pos_bol in
    Printf.eprintf
      "File \"%s\", line %d, characters %d-%d:\n\
       Error: %s\n%!"
      start.pos_fname start.pos_lnum start_c stop_c msg

