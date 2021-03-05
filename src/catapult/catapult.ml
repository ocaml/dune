open Stdune

module Json = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Array of t list
    | Bool of bool
    | Object of (string * t) list

  let quote_string_to_buf s buf =
    (* TODO: escaping is wrong here, in particular for control characters *)
    Buffer.add_string buf (sprintf "%S" s)

  let rec to_buf t buf =
    match t with
    | String s -> quote_string_to_buf s buf
    | Int i -> Buffer.add_string buf (string_of_int i)
    | Float f -> Buffer.add_string buf (string_of_float f)
    | Bool b -> Buffer.add_string buf (string_of_bool b)
    | Array l ->
      Buffer.add_char buf '[';
      array_body_to_buf l buf;
      Buffer.add_char buf ']'
    | Object o ->
      Buffer.add_char buf '{';
      object_body_to_buf o buf;
      Buffer.add_char buf '}'

  and array_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ x ] -> to_buf x buf
    | x :: xs ->
      to_buf x buf;
      Buffer.add_char buf ',';
      array_body_to_buf xs buf

  and object_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ (x, y) ] ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf
    | (x, y) :: xs ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf;
      Buffer.add_char buf ',';
      object_body_to_buf xs buf

  let to_string t =
    let buf = Buffer.create 0 in
    to_buf t buf;
    Buffer.contents buf
end

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> float
  ; gc_stat : unit -> Gc.stat
  ; buffer : Buffer.t
  ; mutable after_first_event : bool
  ; mutable next_id : int
  }

let fake_gc_stat =
  let init_gc = Gc.quick_stat () in
  { init_gc with
    Gc.minor_words = 0.
  ; promoted_words = 0.
  ; major_words = 0.
  ; minor_collections = 0
  ; major_collections = 0
  ; heap_words = 0
  ; heap_chunks = 0
  ; live_words = 0
  ; live_blocks = 0
  ; free_words = 0
  ; free_blocks = 0
  ; largest_free = 0
  ; fragments = 0
  ; compactions = 0
  ; top_heap_words = 0
  ; stack_size = 0
  }
  [@ocaml.warning "-23"]

(* all fields of record used *)

let fake time_ref buf =
  let print s = Buffer.add_string buf s in
  let close () = () in
  let get_time () = !time_ref in
  let gc_stat () = fake_gc_stat in
  let buffer = Buffer.create 1024 in
  { print
  ; close
  ; get_time
  ; gc_stat
  ; after_first_event = false
  ; next_id = 0
  ; buffer
  }

let close { print; close; _ } =
  print "]\n";
  close ()

let make path =
  let channel = Stdlib.open_out path in
  let print s = Stdlib.output_string channel s in
  let close () = Stdlib.close_out channel in
  let get_time () = Unix.gettimeofday () in
  let gc_stat () = Gc.stat () in
  let buffer = Buffer.create 1024 in
  { print
  ; close
  ; get_time
  ; gc_stat
  ; after_first_event = false
  ; next_id = 0
  ; buffer
  }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let pp_time f =
  let n = int_of_float @@ (f *. 1_000_000.) in
  Printf.sprintf "%d" n

type event = int * string

let on_process_end t (id, name) =
  let time = t.get_time () in
  printf t
    {|{"cat": "process", "name": %S, "id": %d, "pid": 0, "ph": "e", "ts": %s}|}
    name id (pp_time time)

let emit_counter t key values =
  let time = t.get_time () in
  let values =
    Buffer.clear t.buffer;
    Json.to_buf (Json.Object values) t.buffer;
    Buffer.contents t.buffer
  in
  printf t {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %s, "args": %s}|}
    key (pp_time time) values

let emit_gc_counters t =
  let stat = t.gc_stat () in
  emit_counter t "gc"
    [ ("live_words", Json.Int stat.live_words)
    ; ("free_words", Int stat.free_words)
    ; ("stack_size", Int stat.stack_size)
    ; ("heap_words", Int stat.heap_words)
    ; ("top_heap_words", Int stat.top_heap_words)
    ; ("minor_words", Float stat.minor_words)
    ; ("major_words", Float stat.major_words)
    ; ("promoted_words", Float stat.promoted_words)
    ; ("compactions", Int stat.compactions)
    ; ("major_collections", Int stat.major_collections)
    ; ("minor_collections", Int stat.minor_collections)
    ]

let next_id t =
  let r = t.next_id in
  t.next_id <- r + 1;
  r

let on_process_start t ~program ~args =
  let name = Filename.basename program in
  let id = next_id t in
  let time = t.get_time () in
  printf t
    {|{"cat": "process", "name": %S, "id": %d, "pid": 0, "ph": "b", "ts": %s, "args": %s}|}
    name id (pp_time time)
    (* TODO: This looks at the very least un-idiomatic: [args] is normally an
       object, but we have it as an array. *)
    (Json.to_string (Array (List.map args ~f:(fun arg -> Json.String arg))));
  (id, name)
