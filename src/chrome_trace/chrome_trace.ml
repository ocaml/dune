module List = ListLabels
module String = StringLabels
open Printf

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

module Timestamp : sig
  type t

  val to_json : t -> Json.t

  val of_float : float -> t
end = struct
  type t = float

  let of_float x = x

  let to_json f =
    let n = int_of_float @@ (f *. 1_000_000.) in
    Json.Int n
end

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> Timestamp.t
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
  let get_time () = Timestamp.of_float !time_ref in
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
  let get_time () = Timestamp.of_float (Unix.gettimeofday ()) in
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

module Event = struct
  [@@@ocaml.warning "-37"]

  type common =
    { name : string
    ; cat : string list
    ; ts : Timestamp.t
    ; tts : Timestamp.t option
    ; pid : int
    ; tid : int
    ; cname : string option
    }

  let common ?tts ?cname ?(cat = []) ~ts ~name ~pid ~tid () =
    { tts; cname; cat; ts; pid; tid; name }

  type scope =
    | Global
    | Process
    | Thread

  type async_kind =
    | Start
    | Instant
    | End

  type args = (string * Json.t) list

  module Id = struct
    type t =
      | Int of int
      | String of string

    let to_json = function
      | Int i -> Json.Int i
      | String s -> Json.String s

    let field id = ("id", to_json id)
  end

  type object_kind =
    | New
    | Snapshot of
        { cat : string list option
        ; args : args
        }
    | Destroy

  type metadata =
    | Process_name of
        { pid : int
        ; name : string
        }
    | Process_labels of
        { pid : int
        ; labels : string
        }
    | Thread_name of
        { tid : int
        ; pid : int
        ; name : string
        }
    | Process_sort_index of
        { pid : int
        ; sort_index : int
        }
    | Thread_sort_index of
        { pid : int
        ; tid : int
        ; sort_index : int
        }

  (* TODO support flow, samples, referemces, memory dumps *)
  type t =
    | Counter of common * args * Id.t option
    | Duration_start of common * args * Id.t option
    | Duration_end of
        { pid : int
        ; tid : int
        ; ts : float
        ; args : args option
        }
    | Complete of
        { common : common
        ; args : args option
        ; dur : Timestamp.t
        ; tdur : Timestamp.t option
        }
    | Instant of common * scope option * args option
    | Async of
        { common : common
        ; async_kind : async_kind
        ; scope : string option
        ; id : Id.t
        ; args : args option
        }
    | Object of
        { common : common
        ; object_kind : object_kind
        ; id : Id.t
        ; scope : string option
        }
    | Metadata of metadata

  let phase s = ("ph", Json.String s)

  let add_field_opt to_field field fields =
    match field with
    | None -> fields
    | Some f -> to_field f :: fields

  let common_fields { name; cat; ts; tts; pid; tid; cname } =
    let fields =
      [ ("name", Json.String name)
      ; ("cat", String (String.concat ~sep:"," cat))
      ; ("ts", Timestamp.to_json ts)
      ; ("pid", Int pid)
      ; ("tid", Int tid)
      ]
    in
    let fields =
      add_field_opt (fun cname -> ("cname", Json.String cname)) cname fields
    in
    add_field_opt (fun tts -> ("tts", Timestamp.to_json tts)) tts fields

  let json_of_scope = function
    | Global -> Json.String "g"
    | Process -> Json.String "p"
    | Thread -> Json.String "t"

  let args_field fields = ("args", Json.Object fields)

  let json_fields_of_metadata m =
    let fields =
      let common pid name = [ ("name", Json.String name); ("pid", Int pid) ] in
      match m with
      | Process_name { pid; name } ->
        args_field [ ("name", Json.String name) ] :: common pid "thread_name"
      | Process_labels { pid; labels } ->
        args_field [ ("labels", Json.String labels) ]
        :: common pid "process_labels"
      | Thread_name { tid; pid; name } ->
        ("tid", Int tid)
        :: args_field [ ("name", Json.String name) ]
        :: common pid "process_name"
      | Process_sort_index { pid; sort_index } ->
        args_field [ ("sort_index", Json.Int sort_index) ]
        :: common pid "process_sort_index"
      | Thread_sort_index { pid; sort_index; tid } ->
        ("tid", Int tid)
        :: args_field [ ("sort_index", Json.Int sort_index) ]
        :: common pid "thread_sort_index"
    in
    phase "M" :: fields

  let to_json_fields : t -> (string * Json.t) list = function
    | Counter (common, args, id) ->
      let fields = common_fields common in
      let fields = phase "C" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_start (common, args, id) ->
      let fields = common_fields common in
      let fields = phase "B" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_end { pid; tid; ts; args } ->
      let fields =
        [ ("tid", Json.Int tid)
        ; ("pid", Int pid)
        ; ("ts", Json.Float ts)
        ; phase "E"
        ]
      in
      add_field_opt args_field args fields
    | Complete { common; dur; args; tdur } ->
      let fields = common_fields common in
      let fields = phase "X" :: ("dur", Timestamp.to_json dur) :: fields in
      let fields =
        add_field_opt (fun tdur -> ("tdur", Timestamp.to_json tdur)) tdur fields
      in
      add_field_opt args_field args fields
    | Instant (common, scope, args) ->
      let fields = common_fields common in
      let fields = phase "i" :: fields in
      let fields =
        add_field_opt (fun s -> ("s", json_of_scope s)) scope fields
      in
      add_field_opt args_field args fields
    | Async { common; async_kind; scope; id; args } ->
      let fields = common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph =
          let s =
            match async_kind with
            | Start -> "b"
            | Instant -> "n"
            | End -> "e"
          in
          phase s
        in
        ph :: fields
      in
      let fields =
        add_field_opt (fun s -> ("scope", Json.String s)) scope fields
      in
      add_field_opt args_field args fields
    | Object { common; object_kind; id; scope } ->
      let fields = common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph, args =
          match object_kind with
          | New -> ("N", None)
          | Destroy -> ("D", None)
          | Snapshot { cat; args } ->
            let snapshot =
              add_field_opt
                (fun cat -> ("cat", Json.String (String.concat ~sep:"," cat)))
                cat args
            in
            ("O", Some [ ("snapshot", Json.Object snapshot) ])
        in
        let fields = phase ph :: fields in
        add_field_opt args_field args fields
      in
      add_field_opt (fun s -> ("scope", Json.String s)) scope fields
    | Metadata m -> json_fields_of_metadata m

  let to_json t = Json.Object (to_json_fields t)
end

type event = Event.Id.t * string

let emit_counter t key values =
  let time = t.get_time () in
  let event =
    let common = Event.common ~name:key ~pid:0 ~tid:0 ~ts:time () in
    Event.Counter (common, values, None)
  in
  printf t "%s" (Json.to_string (Event.to_json event))

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
  Event.Id.Int r

let on_process_start t ~program ~args =
  let name = Filename.basename program in
  let id = next_id t in
  let time = t.get_time () in
  let event =
    let common =
      Event.common ~cat:[ "process" ] ~name ~pid:0 ~tid:0 ~ts:time ()
    in
    let args =
      [ ( "process_args"
        , Json.Array (List.map args ~f:(fun arg -> Json.String arg)) )
      ]
    in
    Event.Async
      { common; async_kind = Start; scope = None; id; args = Some args }
  in
  printf t "%s" (Json.to_string (Event.to_json event));
  (id, name)

let on_process_end t (id, name) =
  let time = t.get_time () in
  let event =
    let common =
      Event.common ~cat:[ "process" ] ~name ~pid:0 ~tid:0 ~ts:time ()
    in
    Event.Async { common; async_kind = Start; scope = None; id; args = None }
  in
  printf t "%s" (Json.to_string (Event.to_json event))
