let flush (module Backend : Backend_intf.S) : Backend_intf.t =
  (module struct
    include Backend

    let print_if_no_status_line msg =
      print_if_no_status_line msg;
      flush stderr

    let print_user_message msg =
      print_user_message msg;
      flush stderr

    let reset () =
      reset ();
      flush stderr

    let reset_flush_history () =
      reset_flush_history ();
      flush stderr
  end : Backend_intf.S)

let compose (module A : Backend_intf.S) (module B : Backend_intf.S) :
    (module Backend_intf.S) =
  (module struct
    let start () =
      A.start ();
      B.start ()

    let print_user_message msg =
      A.print_user_message msg;
      B.print_user_message msg

    let set_status_line x =
      A.set_status_line x;
      B.set_status_line x

    let finish () =
      A.finish ();
      B.finish ()

    let print_if_no_status_line msg =
      A.print_if_no_status_line msg;
      B.print_if_no_status_line msg

    let reset () =
      A.reset ();
      B.reset ()

    let reset_flush_history () =
      A.reset_flush_history ();
      B.reset_flush_history ()
  end : Backend_intf.S)

let signal_usr1_on_pipe (module A : Backend_intf.S) =
  let sent = ref false in
  let wrap f =
    match f () with
    | s -> s
    | exception Unix.Unix_error (Unix.EPIPE, _, _) ->
      if !sent then (
        sent := true;
        Unix.kill (Unix.getpid ()) Sys.sigusr1)
  in
  (module struct
    let print_user_message msg = wrap (fun () -> A.print_user_message msg)

    let set_status_line x = wrap (fun () -> A.set_status_line x)

    let start () = wrap A.start

    let finish () = wrap A.finish

    let print_if_no_status_line msg =
      wrap (fun () -> A.print_if_no_status_line msg)

    let reset () = wrap A.reset

    let reset_flush_history () = wrap A.reset_flush_history
  end : Backend_intf.S)
