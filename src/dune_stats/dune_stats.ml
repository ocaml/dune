open Stdune
module Event = Chrome_trace.Event

module Mac = struct
  external open_fds : pid:int -> int = "dune_stats_open_fds"

  external available : unit -> bool = "dune_stats_available"
end

module Json = struct
  include Chrome_trace.Json

  let quote_string_to_buf s buf =
    (* TODO: escaping is wrong here, in particular for control characters *)
    Buffer.add_string buf (sprintf "%S" s)

  let rec to_buf t buf =
    match t with
    | `String s -> quote_string_to_buf s buf
    | `Int i -> Buffer.add_string buf (string_of_int i)
    | `Float f -> Buffer.add_string buf (Printf.sprintf "%.17g" f)
    | `Bool b -> Buffer.add_string buf (string_of_bool b)
    | `List l ->
      Buffer.add_char buf '[';
      array_body_to_buf l buf;
      Buffer.add_char buf ']'
    | `Assoc o ->
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

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      }

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; mutable after_first_event : bool
  }

(* all fields of record used *)

let close { print; close; _ } =
  print "]\n";
  close ()

let create dst =
  let print =
    match dst with
    | Out out -> Stdlib.output_string out
    | Custom c -> c.write
  in
  let close =
    match dst with
    | Out out -> fun () -> Stdlib.close_out out
    | Custom c -> c.close
  in
  { print; close; after_first_event = false }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let emit t event = printf t "%s" (Json.to_string (Event.to_json event))

module Fd_count = struct
  type t =
    | Unknown
    | This of int

  let lsof =
    let prog = lazy (Bin.which ~path:(Env.path Env.initial) "lsof") in
    (* note: we do not use the Process module here, because it would create a
       circular dependency *)
    fun () ->
      match Lazy.force prog with
      | None -> Unknown
      | Some prog -> (
        let lsof_r, lsof_w = Unix.pipe ~cloexec:true () in
        let prog = Path.to_string prog in
        let pid =
          let argv =
            [ prog
            ; "-l"
            ; "-O"
            ; "-P"
            ; "-n"
            ; "-w"
            ; "-p"
            ; string_of_int (Unix.getpid ())
            ]
          in
          Spawn.spawn ~prog ~argv ~stdout:lsof_w () |> Pid.of_int
        in
        Unix.close lsof_w;
        match
          let _, status = Unix.waitpid [] (Pid.to_int pid) in
          status
        with
        | Unix.WEXITED 0 ->
          let count =
            let chan = Unix.in_channel_of_descr lsof_r in
            let rec loop acc =
              match input_line chan with
              | exception End_of_file -> acc
              | _ -> loop (acc + 1)
            in
            (* the output contains a header line *)
            let res = loop (-1) in
            Io.close_in chan;
            res
          in
          This count
        | (exception Unix.Unix_error (_, _, _))
        (* The final [waitpid] call fails with:

           {[ Error: waitpid(): No child processes ]} *)
        | _ ->
          Unix.close lsof_r;
          Unknown)

  let proc_fs () =
    match Sys.readdir "/proc/self/fd" with
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))
    | exception Sys_error _ -> Unknown

  let how = ref `Unknown

  let pid = lazy (Unix.getpid ())

  let get () =
    match !how with
    | `Disable -> Unknown
    | `Lsof -> lsof ()
    | `Proc_fs -> proc_fs ()
    | `Mac -> This (Mac.open_fds ~pid:(Lazy.force pid))
    | `Unknown -> (
      if Mac.available () then (
        how := `Mac;
        This (Mac.open_fds ~pid:(Lazy.force pid)))
      else
        match proc_fs () with
        | This _ as n ->
          how := `Proc_fs;
          n
        | Unknown ->
          let res = lsof () in
          (how :=
             match res with
             | This _ -> `Lsof
             | Unknown -> `Disable);
          res)
end

let record_gc_and_fd stats =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
  let () =
    let common = Event.common_fields ~name:"gc" ~ts () in
    let args =
      let stat = Gc.quick_stat () in
      [ ("stack_size", `Int stat.stack_size)
      ; ("heap_words", `Int stat.heap_words)
      ; ("top_heap_words", `Int stat.top_heap_words)
      ; ("minor_words", `Float stat.minor_words)
      ; ("major_words", `Float stat.major_words)
      ; ("promoted_words", `Float stat.promoted_words)
      ; ("compactions", `Int stat.compactions)
      ; ("major_collections", `Int stat.major_collections)
      ; ("minor_collections", `Int stat.minor_collections)
      ]
    in
    let event = Event.counter common args in
    emit stats event
  in
  match Fd_count.get () with
  | Unknown -> ()
  | This fds ->
    let event =
      let args = [ ("value", `Int fds) ] in
      let common = Event.common_fields ~name:"fds" ~ts () in
      Event.counter common args
    in
    emit stats event

module Private = struct
  module Fd_count = Fd_count
end
