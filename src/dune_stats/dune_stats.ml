open Stdune
module Timestamp = Chrome_trace.Event.Timestamp
module Event = Chrome_trace.Event

module Mac = struct
  external open_fds : pid:int -> int = "dune_stats_open_fds"
  external available : unit -> bool = "dune_stats_available"
end

module Json = struct
  include Chrome_trace.Json

  let copy_substring s buf start pos =
    if pos > start then Buffer.add_substring buf s start (pos - start)
  ;;

  let rec quote_characters_to_buf s buf n start pos =
    (* check if a character is a valid utf-8 continuation byte *)
    let is_cb i = i < n && Char.code s.[i] land 0xc0 = 0x80 [@@inline] in
    if pos < n
    then (
      match s.[pos] with
      | '\b' -> escape s buf n start pos "\\b"
      | '\t' -> escape s buf n start pos "\\t"
      | '\n' -> escape s buf n start pos "\\n"
      | '\012' -> escape s buf n start pos "\\f"
      | '\r' -> escape s buf n start pos "\\r"
      | '\\' -> escape s buf n start pos "\\\\"
      | '"' -> escape s buf n start pos "\\\""
      | '\000' .. '\031' as c ->
        escape s buf n start pos (sprintf "\\u%04x" (Char.code c))
      | '\032' .. '\127' -> quote_characters_to_buf s buf n start (pos + 1)
      (* Check for valid UTF-8 *)
      | '\xc0' .. '\xdf' when is_cb (pos + 1) ->
        quote_characters_to_buf s buf n start (pos + 2)
      | '\xe0' .. '\xef' when is_cb (pos + 1) && is_cb (pos + 2) ->
        quote_characters_to_buf s buf n start (pos + 3)
      | '\xf0' .. '\xf7' when is_cb (pos + 1) && is_cb (pos + 2) && is_cb (pos + 3) ->
        quote_characters_to_buf s buf n start (pos + 4)
      (* Replace unrepresentable bytes by the Unicode replacement character (0xFFFD),
         encoded in UTF-8 *)
      | _ -> escape s buf n start pos "\xef\xbf\xbd")
    else copy_substring s buf start pos

  and escape s buf n start pos e =
    copy_substring s buf start pos;
    Buffer.add_string buf e;
    quote_characters_to_buf s buf n (pos + 1) (pos + 1)
  ;;

  let quote_string_to_buf s buf =
    Buffer.add_char buf '"';
    quote_characters_to_buf s buf (String.length s) 0 0;
    Buffer.add_char buf '"'
  ;;

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
    | `Null -> Buffer.add_string buf "null"

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
  ;;

  let to_string t =
    let buf = Buffer.create 0 in
    to_buf t buf;
    Buffer.contents buf
  ;;
end

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; flush : unit -> unit
  ; extended_build_job_info : bool
  ; mutable after_first_event : bool
  }

(* all fields of record used *)

let close { print; close; _ } =
  print "]\n";
  close ()
;;

let global = ref None

let () =
  at_exit (fun () ->
    match !global with
    | None -> ()
    | Some t -> close t)
;;

let set_global t =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some t
;;

let global () = !global

let create ~extended_build_job_info dst =
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
  let flush =
    match dst with
    | Out out -> fun () -> flush out
    | Custom c -> c.flush
  in
  { print; close; after_first_event = false; flush; extended_build_job_info }
;;

let flush t = t.flush ()
let extended_build_job_info t = t.extended_build_job_info

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['
;;

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c
;;

let emit t event = printf t "%s" (Json.to_string (Event.to_json event))

type event_data =
  { args : Chrome_trace.Event.args option
  ; cat : string list option
  ; name : string
  }

type event =
  { t : t
  ; event_data : event_data
  ; start : float
  }

let start t k : event option =
  match t with
  | None -> None
  | Some t ->
    let event_data = k () in
    let start = Unix.gettimeofday () in
    Some { t; event_data; start }
;;

let finish event =
  match event with
  | None -> ()
  | Some { t; start; event_data = { args; cat; name } } ->
    let dur =
      let stop = Unix.gettimeofday () in
      Timestamp.of_float_seconds (stop -. start)
    in
    let common =
      Event.common_fields ?cat ~name ~ts:(Timestamp.of_float_seconds start) ()
    in
    let event = Event.complete ?args common ~dur in
    emit t event
;;

module Fd_count = struct
  type t =
    | Unknown
    | This of int

  let lsof =
    let prog = lazy (Bin.which ~path:(Env_path.path Env.initial) "lsof") in
    (* note: we do not use the Process module here, because it would create a
       circular dependency *)
    fun () ->
      match Lazy.force prog with
      | None -> Unknown
      | Some prog ->
        let lsof_r, lsof_w = Unix.pipe ~cloexec:true () in
        let prog = Path.to_string prog in
        let pid =
          let argv =
            [ prog; "-l"; "-O"; "-P"; "-n"; "-w"; "-p"; string_of_int (Unix.getpid ()) ]
          in
          Spawn.spawn ~prog ~argv ~stdout:lsof_w () |> Pid.of_int
        in
        Unix.close lsof_w;
        (match
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

            {[
              Error: waitpid(): No child processes
            ]} *)
         | _ ->
           Unix.close lsof_r;
           Unknown)
  ;;

  let proc_fs () =
    match Sys.readdir "/proc/self/fd" with
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))
    | exception Sys_error _ -> Unknown
  ;;

  let how = ref `Unknown
  let pid = lazy (Unix.getpid ())

  let get () =
    match !how with
    | `Disable -> Unknown
    | `Lsof -> lsof ()
    | `Proc_fs -> proc_fs ()
    | `Mac -> This (Mac.open_fds ~pid:(Lazy.force pid))
    | `Unknown ->
      if Mac.available ()
      then (
        how := `Mac;
        This (Mac.open_fds ~pid:(Lazy.force pid)))
      else (
        match proc_fs () with
        | This _ as n ->
          how := `Proc_fs;
          n
        | Unknown ->
          let res = lsof () in
          (how
           := match res with
              | This _ -> `Lsof
              | Unknown -> `Disable);
          res)
  ;;
end

let record_gc_and_fd stats =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
  let () =
    let common = Event.common_fields ~name:"gc" ~ts () in
    let args =
      let stat = Gc.quick_stat () in
      [ "stack_size", `Int stat.stack_size
      ; "heap_words", `Int stat.heap_words
      ; "top_heap_words", `Int stat.top_heap_words
      ; "minor_words", `Float stat.minor_words
      ; "major_words", `Float stat.major_words
      ; "promoted_words", `Float stat.promoted_words
      ; "compactions", `Int stat.compactions
      ; "major_collections", `Int stat.major_collections
      ; "minor_collections", `Int stat.minor_collections
      ]
    in
    let event = Event.counter common args in
    emit stats event
  in
  match Fd_count.get () with
  | Unknown -> ()
  | This fds ->
    let event =
      let args = [ "value", `Int fds ] in
      let common = Event.common_fields ~name:"fds" ~ts () in
      Event.counter common args
    in
    emit stats event
;;

module Private = struct
  module Fd_count = Fd_count
end
