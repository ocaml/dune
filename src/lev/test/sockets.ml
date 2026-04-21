let%expect_test "server & client" =
  let server_sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  let domain = Unix.domain_of_sockaddr server_sockaddr in
  let socket () = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let loop = Lev.Loop.create () in
  let client_sockaddr = ref None in
  let server_ready =
    let client () =
      let fd = socket () in
      print_endline "client: connecting";
      Unix.connect
        fd
        (match !client_sockaddr with
         | None -> assert false
         | Some s -> s);
      Unix.close fd
    in
    Lev.Async.create (fun async ->
      client ();
      Lev.Async.stop async loop;
      Lev.Async.destroy async)
  in
  Lev.Async.start server_ready loop;
  let () =
    let server_fd = socket () in
    Unix.set_nonblock server_fd;
    Unix.setsockopt server_fd Unix.SO_REUSEADDR true;
    Unix.bind server_fd server_sockaddr;
    Unix.listen server_fd 10;
    client_sockaddr := Some (Unix.getsockname server_fd);
    print_endline "server: listening";
    Lev.Async.send server_ready loop;
    let io =
      Lev.Io.create
        (fun io _ _ ->
           let client_fd, _sockaddr = Unix.accept ~cloexec:true server_fd in
           print_endline "server: accepting client";
           Unix.close client_fd;
           print_endline "server: terminating";
           Lev.Io.stop io loop;
           Unix.close server_fd;
           Lev.Io.destroy io)
        server_fd
        (Lev.Io.Event.Set.create ~read:true ())
    in
    Lev.Io.start io loop
  in
  Lev.Loop.run_until_done loop;
  [%expect
    {|
    server: listening
    client: connecting
    server: accepting client
    server: terminating |}]
;;
