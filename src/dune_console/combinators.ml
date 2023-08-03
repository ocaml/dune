let flush (module Backend : Backend_intf.S) : Backend_intf.t =
  (module struct
    include Backend

    let print_if_no_status_line msg =
      print_if_no_status_line msg;
      flush stderr
    ;;

    let print_user_message msg =
      print_user_message msg;
      flush stderr
    ;;

    let reset () =
      reset ();
      flush stderr
    ;;

    let reset_flush_history () =
      reset_flush_history ();
      flush stderr
    ;;
  end : Backend_intf.S)
;;

let compose (module A : Backend_intf.S) (module B : Backend_intf.S)
  : (module Backend_intf.S)
  =
  (module struct
    let start () =
      A.start ();
      B.start ()
    ;;

    let print_user_message msg =
      A.print_user_message msg;
      B.print_user_message msg
    ;;

    let set_status_line x =
      A.set_status_line x;
      B.set_status_line x
    ;;

    let finish () =
      A.finish ();
      B.finish ()
    ;;

    let print_if_no_status_line msg =
      A.print_if_no_status_line msg;
      B.print_if_no_status_line msg
    ;;

    let reset () =
      A.reset ();
      B.reset ()
    ;;

    let reset_flush_history () =
      A.reset_flush_history ();
      B.reset_flush_history ()
    ;;
  end : Backend_intf.S)
;;
