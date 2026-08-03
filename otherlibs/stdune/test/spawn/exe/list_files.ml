let () =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare |> List.iter print_endline
;;
