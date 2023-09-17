open Stdune

type t =
  { mutable bytes : Bytes.t (* underlying bytes *)
  ; (* the position we can start reading from (until [pos_w]) *)
    mutable pos_r : int
  ; (* the position we can start writing to (until [Bytes.length bytes - 1]) *)
    mutable pos_w : int
  ; (* total number of bytes written to this buffer. 2^63 bytes should be
       enough for anybody *)
    mutable total_written : int
  }

type flush_token = int

(* We can't use [Out_channel] for writes on Linux because we want to disable
   sigpipes. Eventually we'll move to event based IO and ditch the threads,
   so we'll need this anyway *)

let create ~size = { bytes = Bytes.create size; pos_r = 0; pos_w = 0; total_written = 0 }
let length t = t.pos_w - t.pos_r
let max_buffer_size = 65536

let maybe_resize_to_fit t write_size =
  let buf_len = Bytes.length t.bytes in
  let capacity = buf_len - t.pos_w in
  if capacity < write_size
  then (
    let bytes =
      let new_size =
        let needed =
          let free_space = capacity + t.pos_r in
          buf_len + max 0 (write_size - free_space)
        in
        max (min max_buffer_size (buf_len * 2)) needed
      in
      Bytes.create new_size
    in
    let len = length t in
    Bytes.blit ~src:t.bytes ~src_pos:t.pos_r ~dst:bytes ~dst_pos:0 ~len;
    t.bytes <- bytes;
    t.pos_w <- len;
    t.pos_r <- 0)
;;

let write_char_exn t c =
  assert (t.pos_w < Bytes.length t.bytes);
  Bytes.set t.bytes t.pos_w c;
  t.pos_w <- t.pos_w + 1
;;

let write_string_exn t src =
  assert (t.pos_w < Bytes.length t.bytes);
  let len = String.length src in
  Bytes.blit_string ~src ~src_pos:0 ~dst:t.bytes ~dst_pos:t.pos_w ~len;
  t.pos_w <- t.pos_w + len
;;

let read t len =
  let pos_r = t.pos_r + len in
  if pos_r > t.pos_w
  then
    Code_error.raise
      "not enough bytes in buffer"
      [ "len", Dyn.int len; "length", Dyn.int (length t) ];
  t.pos_r <- pos_r;
  t.total_written <- t.total_written + len
;;

let flush_token t = t.total_written + length t
let flushed t token = t.total_written >= token

let write_csexps =
  let rec loop t (csexp : Csexp.t) =
    match csexp with
    | Atom str ->
      write_string_exn t (string_of_int (String.length str));
      write_char_exn t ':';
      write_string_exn t str
    | List e ->
      write_char_exn t '(';
      List.iter ~f:(loop t) e;
      write_char_exn t ')'
  in
  fun t csexps ->
    let length =
      List.fold_left csexps ~init:0 ~f:(fun acc csexp ->
        acc + Csexp.serialised_length csexp)
    in
    maybe_resize_to_fit t length;
    List.iter ~f:(loop t) csexps
;;

let pos t = t.pos_r
let bytes t = t.bytes

let to_dyn ({ bytes; pos_r; pos_w; total_written } as t) =
  let open Dyn in
  record
    [ "total_written", int total_written
    ; "contents", string (Bytes.sub_string bytes ~pos:pos_r ~len:(length t))
    ; "pos_w", int pos_w
    ; "pos_r", int pos_r
    ]
;;

let write_pos t = t.pos_w
let max_write_len t = Bytes.length t.bytes - t.pos_w

let commit_write t ~len =
  let pos_w = t.pos_w + len in
  if pos_w > Bytes.length t.bytes
  then
    Code_error.raise
      "not enough space to commit write"
      [ "len", Dyn.int len; "t", to_dyn t ];
  t.pos_w <- pos_w
;;

let read_char_exn t =
  assert (t.pos_r < t.pos_w);
  let c = Bytes.get t.bytes t.pos_r in
  read t 1;
  c
;;

let read_into_buffer t buf ~max_len =
  let len = min max_len (t.pos_w - t.pos_r) in
  Buffer.add_subbytes buf t.bytes t.pos_r len;
  read t len;
  len
;;
