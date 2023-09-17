let () =
    let open Result in
    print_endline "Registration of Plugin1";
    Queue.add (fun () ->
      let th = Thread.create (fun () ->
        print_endline "Plugin1 is doing something...") () in
      Thread.join th
    ) Registration.todo
