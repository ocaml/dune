open Stdune

(* On Windows, file descriptors are wrappers around handles rather than
   integers. Their addresses can change, so hashes must not depend on them. *)

let%expect_test "file descriptor hashing is stable across minor collections" =
  (* Empty the minor heap before allocating the "file descriptor". *)
  Gc.minor ();
  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  |> Fd.unsafe_of_unix_file_descr
  |> Exn.protectx ~finally:Fd.close ~f:(fun fd ->
    (* Check the hash before doing a minor collection. *)
    let hash_before = Fd.hash fd in
    (* On Windows this promotes and moves the custom descriptor block. *)
    Gc.minor ();
    (* Check the hash after the minor collection. *)
    let hash_after = Fd.hash fd in
    Printf.printf "hash stable: %b" (Int.equal hash_before hash_after));
  [%expect {| hash stable: true |}]
;;
