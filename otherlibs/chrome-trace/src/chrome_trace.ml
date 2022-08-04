module List = ListLabels
module String = StringLabels

module Json = struct
  type t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `List of t list
    | `Bool of bool
    | `Assoc of (string * t) list
    ]
end

module Timestamp : sig
  type t

  val to_json : t -> Json.t

  val of_float_seconds : float -> t

  val to_float_seconds : t -> float
end = struct
  type t = float

  let of_float_seconds x = x

  let to_float_seconds x = x

  let to_json f =
    let n = int_of_float @@ (f *. 1_000_000.) in
    `Int n
end

module Id = struct
  type t =
    [ `Int of int
    | `String of string
    ]

  let create x = x

  let to_string = function
    | `String s -> s
    | `Int i -> string_of_int i

  let to_json (t : t) = (t :> Json.t)

  let field id = ("id", to_json id)
end

module Stack_frame = struct
  module Raw = struct
    type t = string list

    let create t = t

    let to_json t = `List (List.map t ~f:(fun s -> `String s))
  end

  type t =
    { parent : Id.t option
    ; name : string
    ; category : string
    }

  let create ?parent ~name ~category () = { parent; name; category }

  let to_json { parent; name; category } : Json.t =
    let json = [ ("name", `String name); ("category", `String category) ] in
    let json =
      match parent with
      | None -> json
      | Some id -> ("parent", Id.to_json id) :: json
    in
    `Assoc json
end

module Event = struct
  [@@@ocaml.warning "-37"]

  module Timestamp = Timestamp

  type common_fields =
    { name : string
    ; cat : string list
    ; ts : Timestamp.t
    ; tts : Timestamp.t option
    ; pid : int
    ; tid : int
    ; cname : string option
    ; stackframe : [ `Id of Id.t | `Raw of Stack_frame.Raw.t ] option
    }

  let common_fields ?tts ?cname ?(cat = []) ?(pid = 0) ?(tid = 0) ?stackframe
      ~ts ~name () =
    { tts; cname; cat; ts; pid; tid; name; stackframe }

  let set_ts t ts = { t with ts }

  let ts t = t.ts

  type scope =
    | Global
    | Process
    | Thread

  type async =
    | Start
    | Instant
    | End

  type args = (string * Json.t) list

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

  (* TODO support flow, samples, references, memory dumps *)
  type t =
    | Counter of common_fields * args * Id.t option
    | Duration_start of common_fields * args * Id.t option
    | Duration_end of
        { pid : int
        ; tid : int
        ; ts : float
        ; args : args option
        }
    | Complete of
        { common : common_fields
        ; args : args option
        ; dur : Timestamp.t
        ; tdur : Timestamp.t option
        }
    | Instant of common_fields * scope option * args option
    | Async of
        { common : common_fields
        ; async : async
        ; scope : string option
        ; id : Id.t
        ; args : args option
        }
    | Object of
        { common : common_fields
        ; object_kind : object_kind
        ; id : Id.t
        ; scope : string option
        }
    | Metadata of metadata

  let phase s = ("ph", `String s)

  let add_field_opt to_field field fields =
    match field with
    | None -> fields
    | Some f -> to_field f :: fields

  let json_fields_of_common_fields
      { name; cat; ts; tts; pid; tid; cname; stackframe } =
    let fields =
      [ ("name", `String name)
      ; ("cat", `String (String.concat ~sep:"," cat))
      ; ("ts", Timestamp.to_json ts)
      ; ("pid", `Int pid)
      ; ("tid", `Int tid)
      ]
    in
    let fields =
      add_field_opt (fun cname -> ("cname", `String cname)) cname fields
    in
    let fields =
      add_field_opt (fun tts -> ("tts", Timestamp.to_json tts)) tts fields
    in
    add_field_opt
      (fun stackframe ->
        match stackframe with
        | `Id id -> ("sf", Id.to_json id)
        | `Raw r -> ("stack", Stack_frame.Raw.to_json r))
      stackframe fields

  let json_of_scope = function
    | Global -> `String "g"
    | Process -> `String "p"
    | Thread -> `String "t"

  let args_field fields = ("args", `Assoc fields)

  let json_fields_of_metadata m =
    let fields =
      let common pid name = [ ("name", `String name); ("pid", `Int pid) ] in
      match m with
      | Process_name { pid; name } ->
        args_field [ ("name", `String name) ] :: common pid "thread_name"
      | Process_labels { pid; labels } ->
        args_field [ ("labels", `String labels) ] :: common pid "process_labels"
      | Thread_name { tid; pid; name } ->
        ("tid", `Int tid)
        :: args_field [ ("name", `String name) ]
        :: common pid "process_name"
      | Process_sort_index { pid; sort_index } ->
        args_field [ ("sort_index", `Int sort_index) ]
        :: common pid "process_sort_index"
      | Thread_sort_index { pid; sort_index; tid } ->
        ("tid", `Int tid)
        :: args_field [ ("sort_index", `Int sort_index) ]
        :: common pid "thread_sort_index"
    in
    phase "M" :: fields

  let to_json_fields : t -> (string * Json.t) list = function
    | Counter (common, args, id) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "C" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_start (common, args, id) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "B" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_end { pid; tid; ts; args } ->
      let fields =
        [ ("tid", `Int tid); ("pid", `Int pid); ("ts", `Float ts); phase "E" ]
      in
      add_field_opt args_field args fields
    | Complete { common; dur; args; tdur } ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "X" :: ("dur", Timestamp.to_json dur) :: fields in
      let fields =
        add_field_opt (fun tdur -> ("tdur", Timestamp.to_json tdur)) tdur fields
      in
      add_field_opt args_field args fields
    | Instant (common, scope, args) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "i" :: fields in
      let fields =
        add_field_opt (fun s -> ("s", json_of_scope s)) scope fields
      in
      add_field_opt args_field args fields
    | Async { common; async; scope; id; args } ->
      let fields = json_fields_of_common_fields common in
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
      let fields = add_field_opt (fun s -> ("scope", `String s)) scope fields in
      add_field_opt args_field args fields
    | Object { common; object_kind; id; scope } ->
      let fields = json_fields_of_common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph, args =
          match object_kind with
          | New -> ("N", None)
          | Destroy -> ("D", None)
          | Snapshot { cat; args } ->
            let snapshot =
              add_field_opt
                (fun cat -> ("cat", `String (String.concat ~sep:"," cat)))
                cat args
            in
            ("O", Some [ ("snapshot", `Assoc snapshot) ])
        in
        let fields = phase ph :: fields in
        add_field_opt args_field args fields
      in
      add_field_opt (fun s -> ("scope", `String s)) scope fields
    | Metadata m -> json_fields_of_metadata m

  let to_json t = `Assoc (to_json_fields t)

  let counter ?id common args = Counter (common, args, id)

  let complete ?tdur ?args ~dur common = Complete { common; tdur; dur; args }

  let async ?scope ?args id async common =
    Async { common; args; scope; id; async }
end

module Output_object = struct
  type t =
    { displayTimeUnit : [ `Ms | `Ns ] option
    ; traceEvents : Event.t list
    ; stackFrames : (Id.t * Stack_frame.t) list option
    ; extra_fields : (string * Json.t) list option
    }

  let to_json { displayTimeUnit; traceEvents; extra_fields; stackFrames } =
    let json =
      [ ("traceEvents", `List (List.map traceEvents ~f:Event.to_json)) ]
    in
    let json =
      match displayTimeUnit with
      | None -> json
      | Some u ->
        ( "displayTimeUnit"
        , `String
            (match u with
            | `Ms -> "ms"
            | `Ns -> "ns") )
        :: json
    in
    let json : (string * Json.t) list =
      match stackFrames with
      | None -> json
      | Some frames ->
        let frames =
          List.map frames ~f:(fun (id, frame) ->
              let id = Id.to_string id in
              (id, Stack_frame.to_json frame))
        in
        ("stackFrames", `Assoc frames) :: json
    in
    let json =
      match extra_fields with
      | None -> json
      | Some extra_fields -> json @ extra_fields
    in
    `Assoc json

  let create ?displayTimeUnit ?extra_fields ?stackFrames ~traceEvents () =
    { displayTimeUnit; extra_fields; traceEvents; stackFrames }
end
