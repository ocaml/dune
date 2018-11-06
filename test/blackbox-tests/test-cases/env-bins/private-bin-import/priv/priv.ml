
let () =
  let name = Filename.basename Sys.argv.(0) in
  Printf.printf "Executing priv as %s\n" name
