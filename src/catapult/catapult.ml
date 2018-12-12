open Stdune

type ops =
  { print : string -> unit
  ; close : unit -> unit
  }

type state =
  | Disabled
  | Path of string
  | Using of ops

type t =
  { mutable state : state
  }

let make () =
  { state = Disabled
  }

let close t = match t.state with
  | Disabled -> ()
  | Path _ -> ()
  | Using {print; close} ->
    print "]\n";
    close ()

let enable t path =
  t.state <- Path path

let make_reporter path =
  let channel = Pervasives.open_out path in
  let print s = Pervasives.output_string channel s in
  let close () = Pervasives.close_out channel in
  {print; close}

let printf t format_string =
  match t.state with
  | Disabled ->
    Printf.ifprintf () format_string
  | Using {print; _} ->
    Printf.ksprintf print ("," ^^ format_string ^^ "\n")
  | Path path ->
    let reporter = make_reporter path in
    t.state <- Using reporter;
    Printf.ksprintf reporter.print ("[" ^^ format_string ^^ "\n")

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

let emit_process t {start_time; program; args} ~time =
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

let emit_counter t ~time key value =
  printf
    t
    {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %s, "args": {%S: %d}}|}
    key
    (pp_time time)
    "value"
    value

let emit_counters t ~time (stat: Gc.stat) =
  emit_counter t ~time "live_words" stat.live_words;
  emit_counter t ~time "free_words" stat.free_words;
  emit_counter t ~time "stack_size" stat.stack_size

let get_time t = match t.state with
  | Disabled -> 0.
  | Path _
  | Using _
    ->
    Unix.gettimeofday ()

let on_process_start t ~program ~args =
  { start_time = get_time t
  ; program
  ; args
  }

let on_process_end t event =
  let time = get_time t in
  emit_process t event ~time;
  let stat = Gc.stat () in
  emit_counters t stat ~time
