let register s f = print_endline (s^": registering"); f ()

let handle_dynlink_error = function
| Dynlink.Error error ->
  Printf.printf "Error during dynlink: %s\n%!" (Dynlink.error_message error)
| e -> raise e
