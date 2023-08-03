open Stdune

let () = Printexc.record_backtrace false

let%expect_test "user errors are serializable" =
  let loc = Loc.none in
  let annots =
    User_message.Annots.singleton User_message.Annots.has_embedded_location ()
  in
  let error = User_error.make ~loc ~annots [ Pp.text "testing" ] in
  let (_ : string) = Marshal.to_string error [] in
  [%expect {||}]
;;
