(* TODO get rid of this when inverting the deps between stdune and usexp *)

module List = ListLabels
module String = struct
  include StdLabels.String

  let split_on_char s ~on =
    let rec loop i j =
      if j = length s then
        [sub s ~pos:i ~len:(j - i)]
      else if s.[j] = on then
        sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
      else
        loop i (j + 1)
    in
    loop 0 0
end

module Bytes = struct
  include StdLabels.Bytes

  (* [blit_string] was forgotten from the labeled version in OCaml
     4.02—4.04. *)
  let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
    Bytes.blit_string src src_pos dst dst_pos len
end
