let get_value () = Lwt.return 42

let run_promise p = Lwt_main.run p
