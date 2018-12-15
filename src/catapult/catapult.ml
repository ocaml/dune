open Stdune

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> float
  ; gc_stat : unit -> Gc.stat
  ; mutable after_first_event : bool
  }

let fake_gc_stat =
  { Gc.minor_words = 0.
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

let fake time_ref buf =
  let print s = Buffer.add_string buf s in
  let close () = () in
  let get_time () = !time_ref in
  let gc_stat () = fake_gc_stat in
  { print
  ; close
  ; get_time
  ; gc_stat
  ; after_first_event = false
  }

let close {print; close; _} =
  print "]\n";
  close ()

let make path =
  let channel = Pervasives.open_out path in
  let print s = Pervasives.output_string channel s in
  let close () = Pervasives.close_out channel in
  let get_time () = Unix.gettimeofday () in
  let gc_stat () = Gc.stat () in
  {print; close; get_time; gc_stat; after_first_event = false}

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let color_of_name = function
  | "ocamlc" | "ocamlc.opt" -> "thread_state_uninterruptible"
  | "ocamlopt" | "ocamlopt.opt" -> "thread_state_running"
  | "ocamldep" | "ocamldep.opt" -> "thread_state_runnable"
  | "ocamlmklib" | "ocamlmklib.opt" -> "thread_state_unknown"
  | "ocamllex" | "ocamllex.opt" -> "thread_state_sleeping"
  | "ocamlfind" -> "terrible"
  | "ocaml" -> "bad"
  | "odoc" -> "white"
  | "pp.exe"
  | "ppx.exe" -> "yellow"
  | "menhir" -> "olive"
  | "gcc" -> "rail_response"
  | "git" -> "rail_animation"
  | "refmt" -> "rail_idle"
  | "ocamlyacc" -> "rail_load"
  | "sh"
  | "bash" -> "thread_state_iowait"
  | _ -> "generic_work"

let pp_args l =
  l
  |> List.map ~f:(Printf.sprintf "%S")
  |> String.concat ~sep:","
  |> Printf.sprintf "[%s]"

let pp_time f =
  let n = int_of_float @@ f *. 1_000_000. in
  Printf.sprintf "%d" n

type event =
  { start_time : float
  ; program : string
  ; args : string list
  }

let on_process_end t {start_time; program; args} =
  let time = t.get_time () in
  let dur = time -. start_time in
  let name = Filename.basename program in
  printf
    t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "X", "dur": %s, "ts": %s, "color": %S, "args": %s}|}
    name
    (pp_time dur)
    (pp_time time)
    (color_of_name name)
    (pp_args args)

let emit_counter t key value =
  let time = t.get_time () in
  printf
    t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %s, "args": {%S: %d}}|}
    key
    (pp_time time)
    "value"
    value

let emit_gc_counters t =
  let stat = t.gc_stat () in
  emit_counter t "live_words" stat.live_words;
  emit_counter t "free_words" stat.free_words;
  emit_counter t "stack_size" stat.stack_size

let on_process_start t ~program ~args =
  { start_time = t.get_time ()
  ; program
  ; args
  }
