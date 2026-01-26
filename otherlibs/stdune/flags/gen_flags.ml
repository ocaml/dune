let () = Printf.printf @@ if Sys.argv.(1) = "beos" then {|(-cclib -lbsd)|} else "()"
