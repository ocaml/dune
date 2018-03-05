let () = Printf.printf "Test!!\n%!"

let () = if Sys.argv.(1) = "fail" then assert false
