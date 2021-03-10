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

  val now : unit -> t

  val of_float_seconds : float -> t
end = struct
  type t = float

  let now () = Unix.gettimeofday ()

  let of_float_seconds x = x

  let to_json f =
    let n = int_of_float @@ (f *. 1_000_000.) in
    Json.Int n
end

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> Timestamp.t
  ; buffer : Buffer.t
  ; mutable after_first_event : bool
  }

(* all fields of record used *)

let fake time_ref buf =
  let print s = Buffer.add_string buf s in
  let close () = () in
  let get_time () = Timestamp.of_float_seconds !time_ref in
  let buffer = Buffer.create 1024 in
  { print; close; get_time; after_first_event = false; buffer }

let close { print; close; _ } =
  print "]\n";
  close ()

let make path =
  let channel = Stdlib.open_out path in
  let print s = Stdlib.output_string channel s in
  let close () = Stdlib.close_out channel in
  let get_time () = Timestamp.of_float_seconds (Unix.gettimeofday ()) in
  let buffer = Buffer.create 1024 in
  { print; close; get_time; after_first_event = false; buffer }

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

  module Timestamp = Timestamp

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

  let set_ts t ts = { t with ts }

  type scope =
    | Global
    | Process
    | Thread

  type async =
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
        ; async : async
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
        ::
        args_field [ ("name", Json.String name) ] :: common pid "process_name"
      | Process_sort_index { pid; sort_index } ->
        args_field [ ("sort_index", Json.Int sort_index) ]
        :: common pid "process_sort_index"
      | Thread_sort_index { pid; sort_index; tid } ->
        ("tid", Int tid)
        ::
        args_field [ ("sort_index", Json.Int sort_index) ]
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
    | Async { common; async; scope; id; args } ->
      let fields = common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph =
          let s =
            match async with
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

  let counter ?id common args = Counter (common, args, id)

  let complete ?tdur ?args ~dur common = Complete { common; tdur; dur; args }

  let async ?scope ?args id async common =
    Async { common; args; scope; id; async }
end

let emit t event = printf t "%s" (Json.to_string (Event.to_json event))
