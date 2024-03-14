type t =
  { mutable buf : Bytes.t
  ; mutable pos : int
  }

let[@inline] create capacity = { buf = Bytes.create capacity; pos = 0 }

let[@inline] build_exact_exn t =
  if not (Int.equal t.pos (Bytes.length t.buf))
  then Code_error.raise "Stdune.String_builder.build_exact_exn: buffer not full" [];
  let result = Bytes.unsafe_to_string t.buf in
  (* Ensure that [t.buf] doesn't get used again. *)
  t.buf <- Bytes.empty;
  t.pos <- 0;
  result
;;

let[@inline] add_char t c =
  Bytes.set t.buf t.pos c;
  t.pos <- t.pos + 1
;;

let[@inline] add_substring t src ~pos ~len =
  Bytes.blit_string ~src ~src_pos:pos ~dst:t.buf ~dst_pos:t.pos ~len;
  t.pos <- t.pos + len
;;

let[@inline] add_string t src = add_substring t src ~pos:0 ~len:(String.length src)
