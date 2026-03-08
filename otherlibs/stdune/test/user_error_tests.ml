open Stdune

let () = Printexc.record_backtrace false

let%expect_test "user errors are serializable" =
  let loc = Loc.none in
  let error = User_error.make ~loc [ Pp.text "testing" ] in
  let (_ : string) = Marshal.to_string error [] in
  [%expect {||}]
;;
