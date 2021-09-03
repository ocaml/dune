open Import

module Id = struct
  module T = struct
    type t = Sexp.t

    let equal = Poly.equal

    let compare = Poly.compare

    let to_dyn s = Sexp.to_dyn s
  end

  include T

  let make s = s

  let to_sexp t = t

  let sexp = Conv.sexp

  let gen f = Conv.field "id" (f sexp)

  let required_field = gen Conv.required

  let optional_field = gen Conv.optional

  let hash = Poly.hash

  module C = Comparable.Make (T)
  module Set = C.Set
end

module Version = struct
  type t = int * int

  let latest = (3, 0)

  let sexp : t Conv.value =
    let open Conv in
    pair int int
end

module Call = struct
  type t =
    { method_ : string
    ; params : Sexp.t
    }

  let to_dyn { method_; params } =
    let open Dyn.Encoder in
    record [ ("method_", String method_); ("params", Sexp.to_dyn params) ]

  let create ?(params = Sexp.List []) ~method_ () = { method_; params }

  let fields =
    let open Conv in
    let to_ (method_, params) = { method_; params } in
    let from { method_; params } = (method_, params) in
    let method_ = field "method" (required string) in
    let params = field "params" (required sexp) in
    iso (both method_ params) to_ from
end

module Request = struct
  type t = Id.t * Call.t
end

module Response = struct
  module Error = struct
    type kind =
      | Invalid_request
      | Code_error
      | Version_error

    let dyn_of_kind =
      let open Dyn.Encoder in
      function
      | Invalid_request -> constr "Invalid_request" []
      | Code_error -> constr "Code_error" []
      | Version_error -> constr "Version_error" []

    type t =
      { payload : Sexp.t option
      ; message : string
      ; kind : kind
      }

    let payload t = t.payload

    let kind t = t.kind

    let message t = t.message

    exception E of t

    let create ?payload ~kind ~message () = { payload; message; kind }

    let of_conv (error : Conv.error) =
      let make_payload = function
        | [] -> None
        | payload -> Some (Sexp.record payload)
      in
      match error with
      | Parse_error { payload; message } ->
        { message; payload = make_payload payload; kind = Invalid_request }
      | Version_error { payload; message; since = _; until = _ } ->
        { message; payload = make_payload payload; kind = Version_error }

    let sexp =
      let open Conv in
      let id = field "payload" (optional sexp) in
      let message = field "message" (required string) in
      let kind =
        field "kind"
          (required
             (enum
                [ ("Invalid_request", Invalid_request)
                ; ("Code_error", Code_error)
                ; ("Version_error", Version_error)
                ]))
      in
      record
        (iso (three id message kind)
           (fun (payload, message, kind) -> { payload; message; kind })
           (fun { payload; message; kind } -> (payload, message, kind)))

    let to_dyn { payload; message; kind } =
      let open Dyn.Encoder in
      record
        [ ("payload", option Sexp.to_dyn payload)
        ; ("message", string message)
        ; ("kind", dyn_of_kind kind)
        ]

    let () =
      Printexc.register_printer (function
        | E e ->
          Some (Dyn.to_string (Dyn.Encoder.constr "Response.E" [ to_dyn e ]))
        | _ -> None)
  end

  type t = (Sexp.t, Error.t) result

  let result =
    let open Conv in
    let ok = constr "ok" sexp (fun x -> Ok x) in
    let error = constr "error" Error.sexp (fun x -> Error x) in
    sum [ econstr ok; econstr error ] (function
      | Ok s -> case s ok
      | Error e -> case e error)

  let fields =
    let open Conv in
    let id = Id.required_field in
    let payload = field "result" (required result) in
    both id payload

  let to_dyn = Result.to_dyn Sexp.to_dyn Error.to_dyn
end

module Initialize = struct
  module Request = struct
    type t =
      { version : int * int
      ; id : Id.t
      }

    let version t = t.version

    let id t = t.id

    let create ~id =
      let version =
        let major, minor = Version.latest in
        if (major, minor) < (3, 1) then
          (-major, minor)
        else
          Version.latest
      in
      { version; id }

    let method_name = "initialize"

    let sexp =
      let open Conv in
      let version = field "version" (required Version.sexp) in
      let id = Id.required_field in
      let to_ (version, id) = { version; id } in
      let from { version; id } = (version, id) in
      record (iso (both version id) to_ from)

    let of_call { Call.method_; params } ~version =
      if String.equal method_ method_name then
        Conv.of_sexp sexp ~version params
        |> Result.map_error ~f:Response.Error.of_conv
      else
        let message = "initialize request expected" in
        Error (Response.Error.create ~message ~kind:Invalid_request ())

    let to_call t =
      let params = Conv.to_sexp sexp t in
      { Call.method_ = "initialize"; params }
  end

  module Response = struct
    type t =
      | Compatibility
      | Supports_versioning of unit

    let sexp =
      let open Conv in
      let real_sexp = record (field "supports_versioning" (required unit)) in
      iso
        (either_untagged unit real_sexp)
        (function
          | Left () -> Compatibility
          | Right () -> Supports_versioning ())
        (function
          | Compatibility -> Left ()
          | Supports_versioning () -> Right ())

    (* We should never send the [Compatibility] variant, since it's for
       backwards compatibility with Dune servers that predate the runtime
       versioning. *)
    let create () = Supports_versioning ()

    let to_response t = Conv.to_sexp sexp t
  end
end

module Version_negotiation = struct
  module Request = struct
    type t = Menu of (string * int list) list

    let method_name = "version_menu"

    let create menu = Menu menu

    let sexp =
      Conv.(
        iso
          (list (pair string (list int)))
          (fun x -> Menu x)
          (function
            | Menu x -> x))

    let to_call t =
      let params = Conv.to_sexp sexp t in
      { Call.method_ = method_name; params }

    let of_call { Call.method_; params } ~version =
      if String.equal method_ method_name then
        Conv.of_sexp sexp ~version params
        |> Result.map_error ~f:Response.Error.of_conv
      else
        let message = "version negotiation request expected" in
        Error (Response.Error.create ~message ~kind:Invalid_request ())
  end

  module Response = struct
    type t = Selected of (string * int) list

    let sexp =
      Conv.(
        iso
          (list (pair string int))
          (fun x -> Selected x)
          (function
            | Selected x -> x))

    let create x = Selected x

    let to_response t = Conv.to_sexp sexp t
  end
end

module Persistent = struct
  module Out = struct
    type t =
      | Packet of Sexp.t
      | Close_connection

    let sexp =
      let open Conv in
      let packet = constr "packet" sexp (fun p -> Packet p) in
      let close_connection =
        constr "close_connection" unit (fun () -> Close_connection)
      in
      sum [ econstr packet; econstr close_connection ] (function
        | Packet p -> case p packet
        | Close_connection -> case () close_connection)
  end

  module In = struct
    type t =
      | New_connection
      | Packet of Csexp.t
      | Close_connection

    let to_dyn =
      let open Dyn.Encoder in
      function
      | New_connection -> constr "New_connection" []
      | Close_connection -> constr "Close_connection" []
      | Packet sexp -> constr "Packet" [ Sexp.to_dyn sexp ]

    let sexp =
      let open Conv in
      let new_connection =
        constr "new_connection" unit (fun () -> New_connection)
      in
      let packet = constr "packet" sexp (fun p -> Packet p) in
      let close_connection =
        constr "close_connection" unit (fun () -> Close_connection)
      in
      sum [ econstr new_connection; econstr packet; econstr close_connection ]
        (function
        | New_connection -> case () new_connection
        | Packet p -> case p packet
        | Close_connection -> case () close_connection)
  end
end

module Packet = struct
  module Query = struct
    type t =
      | Request of Request.t
      | Notification of Call.t

    let sexp =
      let open Conv in
      let to_ (id, payload) =
        match id with
        | None -> Notification payload
        | Some id -> Request (id, payload)
      in
      let from = function
        | Request (id, payload) -> (Some id, payload)
        | Notification payload -> (None, payload)
      in
      record
        (iso
           (let id = Id.optional_field in
            both id Call.fields)
           to_ from)
  end

  module Reply = struct
    type t =
      | Response of (Id.t * Response.t)
      | Notification of Call.t

    let sexp =
      let f = function
        | Either.Left (id, resp) -> Response (id, resp)
        | Right y -> Notification y
      in
      let t = function
        | Response (id, resp) -> Either.Left (id, resp)
        | Notification r -> Right r
      in
      let open Conv in
      record (iso (either Response.fields Call.fields) f t)
  end
end

module Decl = struct
  type 'gen t =
    { method_ : string
    ; key : 'gen Int.Map.t Stdune.Univ_map.Key.t
    }

  module Generation = struct
    type ('wire_req, 'wire_resp, 'real_req, 'real_resp) conv =
      { req : 'wire_req Conv.value
      ; resp : 'wire_resp Conv.value
      ; upgrade_req : 'wire_req -> 'real_req
      ; downgrade_req : 'real_req -> 'wire_req
      ; upgrade_resp : 'wire_resp -> 'real_resp
      ; downgrade_resp : 'real_resp -> 'wire_resp
      }

    type (_, _) t =
      | T :
          ('wire_req, 'wire_resp, 'real_req, 'real_resp) conv
          -> ('real_req, 'real_resp) t
  end

  module Request = struct
    module type S = sig
      type req

      type resp

      type wire_req

      type wire_resp

      val version : int

      val req : wire_req Conv.value

      val resp : wire_resp Conv.value

      val upgrade_req : wire_req -> req

      val downgrade_req : req -> wire_req

      val upgrade_resp : wire_resp -> resp

      val downgrade_resp : resp -> wire_resp
    end

    type ('req, 'resp) gen = int * ('req, 'resp) Generation.t

    let make_gen (type req resp)
        (module M : S with type req = req and type resp = resp) =
      let open M in
      ( version
      , Generation.T
          { req
          ; resp
          ; upgrade_req
          ; downgrade_req
          ; upgrade_resp
          ; downgrade_resp
          } )

    let gen_to_dyn _ = Dyn.String "<generation>"

    type ('req, 'resp) witness = ('req, 'resp) Generation.t t

    type nonrec ('req, 'resp) t =
      { decl : ('req, 'resp) witness
      ; generations : ('req, 'resp) gen list
      }

    let make ~method_ ~generations =
      { generations
      ; decl =
          { method_
          ; key =
              Stdune.Univ_map.Key.create ~name:method_
                (Int.Map.to_dyn gen_to_dyn)
          }
      }
  end

  module Notification = struct
    module type S = sig
      type model

      type wire

      val version : int

      val sexp : wire Conv.value

      val upgrade : wire -> model

      val downgrade : model -> wire
    end

    type 'payload gen = int * ('payload, unit) Generation.t

    let make_gen (type payload) (module M : S with type model = payload) =
      let open M in
      let id x = x in
      ( version
      , Generation.T
          { req = sexp
          ; resp = Conv.unit
          ; upgrade_req = upgrade
          ; downgrade_req = downgrade
          ; upgrade_resp = id
          ; downgrade_resp = id
          } )

    let gen_to_dyn _ = Dyn.String "<generation>"

    type 'payload witness = ('payload, unit) Generation.t t

    type nonrec 'payload t =
      { decl : 'payload witness
      ; generations : 'payload gen list
      }

    let make ~method_ ~generations =
      { generations
      ; decl =
          { method_
          ; key =
              Stdune.Univ_map.Key.create ~name:method_
                (Int.Map.to_dyn gen_to_dyn)
          }
      }
  end

  type ('a, 'b) request = ('a, 'b) Request.t

  type 'a notification = 'a Notification.t
end

module type Fiber = sig
  type 'a t

  val return : 'a -> 'a t

  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

  val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

  val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module Ivar : sig
    type 'a fiber

    type 'a t

    val create : unit -> 'a t

    val read : 'a t -> 'a fiber

    val fill : 'a t -> 'a -> unit fiber
  end
  with type 'a fiber := 'a t
end

module type Sys = sig
  type 'a fiber

  val getenv : string -> string option

  val is_win32 : unit -> bool

  val read_file : string -> string fiber

  val readlink : string -> string option fiber

  val analyze_path : string -> [ `Unix_socket | `Normal_file | `Other ] fiber
end

(* Internal RPC request types *)

module Build_outcome = struct
  type t =
    | Success
    | Failure

  let sexp = Conv.enum [ ("Success", Success); ("Failure", Failure) ]
end

module Status = struct
  module Menu = struct
    type t =
      | Uninitialized
      | Menu of (string * int) list
      | Compatibility

    let sexp =
      let open Conv in
      let menu = constr "menu" (list (pair string int)) (fun m -> Menu m) in
      let compat = constr "compatibility" unit (fun () -> Compatibility) in
      let uninitialized = constr "stage1" unit (fun () -> Uninitialized) in
      let variants = [ econstr menu; econstr compat ] in
      sum variants (function
        | Uninitialized -> case () uninitialized
        | Menu m -> case m menu
        | Compatibility -> case () compat)
  end

  type t = { clients : (Id.t * Menu.t) list }

  let sexp =
    let open Conv in
    let to_ clients = { clients } in
    let from { clients } = clients in
    iso (list (pair Id.sexp Menu.sexp)) to_ from
end
