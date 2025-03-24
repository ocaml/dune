let version =
  match Build_info.V1.version () with
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v

let () = match String.split_on_char '-' version with
  | [tag; plus; _commit; dirty] -> Printf.printf "%s-%s-%s-%s" tag plus "xxxxx" dirty
  | [ x ] -> print_endline x
  | _ -> print_endline "unexpected"
