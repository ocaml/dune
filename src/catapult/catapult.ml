open Stdune

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> float
  ; gc_stat : unit -> Gc.stat
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
  } [@ocaml.warning "-23"]

(* all fiels of record used *)

let fake time_ref buf =
  let print s = Buffer.add_string buf s in
  let close () = () in
  let get_time () = !time_ref in
  let gc_stat () = fake_gc_stat in
  { print; close; get_time; gc_stat; after_first_event = false; next_id = 0 }

let close { print; close; _ } =
  print "]\n";
  close ()

let make path =
  let channel = Stdlib.open_out path in
  let print s = Stdlib.output_string channel s in
  let close () = Stdlib.close_out channel in
  let get_time () = Unix.gettimeofday () in
  let gc_stat () = Gc.stat () in
  { print; close; get_time; gc_stat; after_first_event = false; next_id = 0 }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let pp_args l =
  l
  |> List.map ~f:(Printf.sprintf "%S")
  |> String.concat ~sep:"," |> Printf.sprintf "[%s]"

let pp_time f =
  let n = int_of_float @@ (f *. 1_000_000.) in
  Printf.sprintf "%d" n

type event = int * string

let on_process_end t (id, name) =
  let time = t.get_time () in
  printf t
    {|{"cat": "process", "name": %S, "id": %d, "pid": 0, "ph": "e", "ts": %s}|}
    name id (pp_time time)

let gen_emit_counter t key pvalue value =
  let time = t.get_time () in
  printf t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %s, "args": {%S: %a}}|}
    key (pp_time time) "value" pvalue value

let emit_counter t key value =
  gen_emit_counter t key (Fun.const Int.to_string) value

let emit_counter_float =
  let float () f = Printf.sprintf "%.2f" f in
  fun t key value -> gen_emit_counter t key float value

let emit_gc_counters t =
  let stat = t.gc_stat () in
  emit_counter t "live_words" stat.live_words;
  emit_counter t "free_words" stat.free_words;
  emit_counter t "stack_size" stat.stack_size;
  emit_counter t "heap_words" stat.heap_words;
  emit_counter t "top_heap_words" stat.top_heap_words;
  emit_counter_float t "minor_words" stat.minor_words;
  emit_counter_float t "major_words" stat.major_words;
  emit_counter_float t "promoted_words" stat.promoted_words;
  emit_counter t "compactions" stat.compactions;
  emit_counter t "major_collections" stat.major_collections;
  emit_counter t "minor_collections" stat.minor_collections

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
    name id (pp_time time) (pp_args args);
  (id, name)
