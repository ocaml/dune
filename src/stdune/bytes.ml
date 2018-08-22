(* [blit_string] was forgotten from the labeled version in OCaml
   4.02—4.04. *)
include StdLabels.Bytes

let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
  Caml.Bytes.blit_string src src_pos dst dst_pos len

let sub_string dst ~pos ~len =
  Caml.Bytes.sub_string dst pos len
