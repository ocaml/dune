open Stdune

let enabled = ref false

module Fd_count = struct
  type t = Unknown | This of int

  let try_to_use_lsof () =
    (* note: we do not use the Process module here, because it would
       create a circular dependency *)
    let temp = Filename.temp_file "dune" ".lsof" in
    let stdout =
      Unix.openfile temp [O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE] 0o666
    in
    let prog = "/usr/sbin/lsof" in
    let argv = [prog; "-w"; "-p"; string_of_int (Unix.getpid ())] in
    let pid = Spawn.spawn ~prog ~argv ~stdout () in
    Unix.close stdout;
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
      let num_lines = List.length (Io.input_lines (open_in temp)) in
      This (num_lines - 1) (* the output contains a header line *)
    | _ -> Unknown

  let get () =
    match Sys.readdir "/proc/self/fd" with
    | exception _ ->
      begin match try_to_use_lsof () with
      | exception _ -> Unknown
      | value -> value
      end
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))

  let map2 ~f a b =
    match a, b with
    | Unknown, x | x, Unknown -> x
    | This a, This b -> This (f a b)

  let max = map2 ~f:max

  let to_string = function
    | Unknown -> "unknown"
    | This n -> string_of_int n
end

type t =
  { mutable fds : Fd_count.t
  }

let observed_max =
  { fds = Unknown
  }

let record () =
  if !enabled then begin
    let fds = Fd_count.get () in
    observed_max.fds <- Fd_count.max fds observed_max.fds
  end

let dump () =
  let pr fmt = Printf.eprintf (fmt ^^ "\n") in
  pr "Stats:";
  pr "max opened fds: %s" (Fd_count.to_string observed_max.fds);
  flush stderr

let enable () =
  enabled := true;
  at_exit dump

module Catapult = struct
  type active_state = Pervasives.out_channel * Format.formatter

  type state =
    | Disabled
    | Path of string
    | Active of active_state

  let state = ref Disabled

  let dispose () =
    match !state with
    | Disabled -> ()
    | Path _ -> ()
    | Active (channel, _) ->
      Pervasives.output_string channel "]\n";
      Pervasives.flush channel;
      Pervasives.close_out channel

  let enable path =
    state := Path path;
    at_exit dispose

  let fmt () =
    match !state with
    | Disabled -> None
    | Active (_, fmt) ->
      Format.pp_print_string fmt ",";
      Some fmt
    | Path p ->
      let channel = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 p in
      let fmt = Format.formatter_of_out_channel channel in
      Format.pp_print_string fmt "[";
      state := Active (channel, fmt);
      Some fmt

  let printf format_string =
    match fmt () with
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

  let emit_process {start_time; program; args} ~time =
    let dur = time -. start_time in
    let name = Filename.basename program in
    printf
      {|{"name": %S, "pid": 0, "tid": 0, "ph": "X", "dur": %a, "ts": %a, "color": %S, "args": %a}|}
      name
      pp_time dur
      pp_time time
      (color_of_name name)
      pp_args args

  let emit_counter ~time key value =
    printf
      {|{"name": %S, "pid": 0, "tid": 0, "ph": "C", "ts": %a, "args": {%S: %d}}|}
      key
      pp_time time
      "value"
      value

  let emit_counters ~time (stat: Gc.stat) =
    emit_counter ~time "live_words" stat.live_words;
    emit_counter ~time "free_words" stat.free_words;
    emit_counter ~time "stack_size" stat.stack_size

  let on_process_start ~program ~args =
    { start_time = Unix.gettimeofday ()
    ; program
    ; args
    }

  let on_process_end event =
    let time = Unix.gettimeofday () in
    emit_process event ~time;
    let stat = Gc.stat () in
    emit_counters stat ~time
end
