open Stdune

type t =
  { mutable buf : Bigstringaf.t
  ; mutable pos : int
  }

let create size =
  let buf = Bigstringaf.create size in
  { buf; pos = 0 }
;;

let max_size t = Bigstringaf.length t.buf
let available t = Bigstringaf.length t.buf - t.pos
let clear t = t.pos <- 0
let to_string t = Bigstringaf.substring t.buf ~off:0 ~len:t.pos
let buf t = t.buf
let pos t = t.pos

let add_char t c =
  Bigstringaf.set t.buf t.pos c;
  t.pos <- t.pos + 1
;;

let add_string t str =
  let len = String.length str in
  Bigstringaf.blit_from_string str t.buf ~dst_off:t.pos ~src_off:0 ~len;
  t.pos <- t.pos + len
;;

let drop t n =
  assert (t.pos >= n);
  let buf = Bigstringaf.create (Bigstringaf.length t.buf) in
  Bigstringaf.blit t.buf ~src_off:n buf ~dst_off:0 ~len:(t.pos - n);
  t.buf <- buf;
  t.pos <- t.pos - n
;;

let resize t new_size =
  assert (t.pos <= new_size);
  let buf = Bigstringaf.create new_size in
  Bigstringaf.blit t.buf buf ~src_off:0 ~dst_off:0 ~len:t.pos;
  t.buf <- buf
;;
