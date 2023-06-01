let () =
  print_endline "Registration of Plugin2";
  Queue.add (fun () -> print_endline "Plugin2 is doing something...") Registration.todo
