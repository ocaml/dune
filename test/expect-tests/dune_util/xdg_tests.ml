(* [Dune_util.xdg] is a lazy value. Here we make sure it is un-forced when
   simply linking with the rest of dune.

   There is nothing to be changed here, only form the Dune file. See the
   instructions there. *)

let%expect_test "check xdg has been forced" =
  Dune_util.xdg |> Lazy.is_val |> Printf.printf "forced: %b";
  [%expect {| forced: false |}]
;;
