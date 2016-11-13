open Import

let internal argv =
  match Array.to_list argv with
  | [_; "findlib-packages"] ->
    List.iter (Findlib.all_packages ()) ~f:(Printf.printf "%s\n")
  | _ ->
    ()

let () =
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
