open Stdune

type t =
  { fd : Unix.file_descr
  ; buf : Buffer.t
  ; cats : Category.Set.t
  }

(* CR-someday rgrinberg: remove this once we drop support for < 5.2 *)
external write_bigstring
  :  Unix.file_descr
  -> Bigstringaf.t
  -> int
  -> int
  -> int
  = "dune_trace_write"

let flush =
  (* This loop will almost always result in a single write, but we make sure to
     write everything (albeit inefficiently) if the user is running out of disk
     space, is on NFS, or some exotic operation system that doesn't give us
     atomic writes with [O_APPEND] *)
  let rec loop t pos len =
    if len = 0
    then Buffer.clear t.buf
    else (
      match write_bigstring t.fd (Buffer.buf t.buf) pos (Buffer.pos t.buf - pos) with
      | n -> loop t (pos + n) (len - n)
      | exception e ->
        let () =
          (* inefficient, but we just want to make sure we're preserving the
             invariants of [t] even if we fail for some odd reason. *)
          Buffer.drop t.buf pos
        in
        raise e)
  in
  fun t -> loop t 0 (Buffer.pos t.buf)
;;

let close t =
  flush t;
  Unix.close t.fd
;;

let create cats path =
  let fd =
    Unix.openfile
      (Path.to_string path)
      [ O_TRUNC; O_APPEND; O_CLOEXEC; O_WRONLY; O_CREAT ]
      0o644
  in
  let cats = Category.Set.of_list cats in
  let buf = Buffer.create (1 lsl 16) in
  { fd; cats; buf }
;;

let to_buffer t sexp =
  let rec loop = function
    | Csexp.Atom str ->
      Buffer.add_string t.buf (string_of_int (String.length str));
      Buffer.add_char t.buf ':';
      Buffer.add_string t.buf str
    | List e ->
      Buffer.add_char t.buf '(';
      List.iter ~f:loop e;
      Buffer.add_char t.buf ')'
  in
  loop sexp
;;

let emit_buffered t event =
  let needed = Csexp.serialised_length event in
  if Buffer.available t.buf < needed
  then (
    let new_size = max (Buffer.pos t.buf + needed) (2 * Buffer.max_size t.buf) in
    Buffer.resize t.buf new_size);
  to_buffer t event
;;

let emit t event =
  emit_buffered t event;
  flush t
;;

let start t k : Event.Async.t option =
  match t with
  | None -> None
  | Some _ ->
    let event_data = k () in
    let start = Time.now () in
    Some (Event.Async.create ~event_data ~start)
;;

let finish t event =
  match event with
  | None -> ()
  | Some { Event.Async.start; event_data = { args; cat; name } } ->
    let dur =
      let stop = Time.now () in
      Time.diff stop start
    in
    let event = Event.Event.complete ?args ~start ~dur cat ~name in
    emit t event
;;
