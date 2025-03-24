(* This can be used when testing formatting rules instead of calling the real
   ocamlformat binary. But when doing so, be careful not to expose it to too
   many tests because it will be also be used by `@fmt` in dune itself.
*)

let () =
  let args =
    Sys.argv
    |> Array.to_list
    |> List.tl
    |> List.map (fun s -> Printf.sprintf "%S" s)
    |> String.concat " "
  in
  Printf.eprintf "fake ocamlformat is running: %s\n" args;
  Printf.printf "(* fake ocamlformat output *)"
;;
