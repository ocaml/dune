open Stdune

type active_state = Pervasives.out_channel * Format.formatter

type state =
  | Disabled
  | Path of string
  | Active of active_state

type t =
  { mutable state : state
  }

let make () =
  { state = Disabled
  }

let close t = match t.state with
  | Disabled -> ()
  | Path _ -> ()
  | Active (channel, _) ->
    Pervasives.output_string channel "]\n";
    Pervasives.flush channel;
    Pervasives.close_out channel

let enable t path =
  t.state <- Path path

let fmt t =
  match t.state with
  | Disabled -> None
  | Active (_, fmt) ->
    Format.pp_print_string fmt ",";
    Some fmt
  | Path p ->
    let channel = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 p in
    let fmt = Format.formatter_of_out_channel channel in
    Format.pp_print_string fmt "[";
    t.state <- Active (channel, fmt);
    Some fmt

let printf t format_string =
  match fmt t with
  | Some fmt -> Format.fprintf fmt (format_string ^^ "\n%!")
  | None -> Format.ifprintf Format.std_formatter format_string

let color_of_name = function
  | "ocamlc.opt" -> "thread_state_uninterruptible"
  | "ocamlopt.opt" -> "thread_state_running"
  | "ocamldep.opt" -> "thread_state_runnable"
  | "ocamlmklib.opt" -> "thread_state_unknown"
  | "ocamllex.opt" -> "thread_state_sleeping"
  | "ocamlfind" -> "terrible"
  | "ocaml" -> "bad"
  | "ocamlc" -> "black"
  | "ocamldep" -> "grey"
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

let pp_args fmt l =
  let pp_sep fmt () = Format.pp_print_string fmt "," in
  Format.fprintf fmt "[%a]" (Fmt.list ~pp_sep Fmt.quoted) l

let pp_time fmt f =
  let n = int_of_float @@ f *. 1_000_000. in
  Format.pp_print_int fmt n

type event =
  { start_time : float
  ; program : string
  ; args : string list
  }

let emit_process t {start_time; program; args} ~time =
  let dur = time -. start_time in
  let name = Filename.basename program in
  printf
    t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "X", "dur": %a, "ts": %a, "color": %S, "args": %a}|}
    name
    pp_time dur
    pp_time time
    (color_of_name name)
    pp_args args

let emit_counter t ~time key value =
  printf
    t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %a, "args": {%S: %d}}|}
    key
    pp_time time
    "value"
    value

let emit_counters t ~time (stat: Gc.stat) =
  emit_counter t ~time "live_words" stat.live_words;
  emit_counter t ~time "free_words" stat.free_words;
  emit_counter t ~time "stack_size" stat.stack_size

let on_process_start ~program ~args =
  { start_time = Unix.gettimeofday ()
  ; program
  ; args
  }

let on_process_end t event =
  let time = Unix.gettimeofday () in
  emit_process t event ~time;
  let stat = Gc.stat () in
  emit_counters t stat ~time
