(* [blit_string] was forgotten from the labeled version in OCaml
   4.02—4.04. *)
[@@@ocaml.warning "-32"]
let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
  Caml.Bytes.blit_string src src_pos dst dst_pos len

include StdLabels.Bytes
