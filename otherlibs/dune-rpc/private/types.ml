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
  module Map = C.Map
end

module Version = struct
  type t = int * int

  let latest = 3, 18

  let sexp : t Conv.value =
    let open Conv in
    pair int int
  ;;
end

module Method = struct
  module Name = struct
    type t = string

    let sexp : t Conv.value = Conv.string

    module Map = String.Map
    module Table = String.Table
  end

  module Version = struct
    type t = int

    let sexp = Conv.int

    module Set = Int.Set
    module Map = Int.Map
  end
end

module Call = struct
  type t =
    { method_ : Method.Name.t
    ; params : Sexp.t
    }

  let to_dyn { method_; params } =
    let open Dyn in
    record [ "method_", String method_; "params", Sexp.to_dyn params ]
  ;;

  let create ?(params = Sexp.List []) ~method_ () = { method_; params }

  let fields =
    let open Conv in
    let to_ (method_, params) = { method_; params } in
    let from { method_; params } = method_, params in
    let method_ = field "method" (required Method.Name.sexp) in
    let params = field "params" (required sexp) in
    iso (both method_ params) to_ from
  ;;
end

module Request = struct
  type t = Id.t * Call.t
end

module Response = struct
  module Error = struct
    type kind =
      | Invalid_request
      | Code_error
      | Connection_dead

    let dyn_of_kind =
      let open Dyn in
      function
      | Invalid_request -> variant "Invalid_request" []
      | Code_error -> variant "Code_error" []
      | Connection_dead -> variant "Connection_dead" []
    ;;

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
        (* cwong: Should we even still include this? *)
        { message; payload = make_payload payload; kind = Code_error }
    ;;

    let sexp =
      let open Conv in
      let id = field "payload" (optional sexp) in
      let message = field "message" (required string) in
      let kind =
        field
          "kind"
          (required
             (enum [ "Invalid_request", Invalid_request; "Code_error", Code_error ]))
      in
      record
        (iso
           (three id message kind)
           (fun (payload, message, kind) -> { payload; message; kind })
           (fun { payload; message; kind } -> payload, message, kind))
    ;;

    let to_dyn { payload; message; kind } =
      let open Dyn in
      record
        [ "payload", option Sexp.to_dyn payload
        ; "message", string message
        ; "kind", dyn_of_kind kind
        ]
    ;;

    let () =
      Printexc.register_printer (function
        | E e -> Some (Dyn.to_string (Dyn.variant "Response.E" [ to_dyn e ]))
        | _ -> None)
    ;;
  end

  type t = (Sexp.t, Error.t) result

  let result =
    let open Conv in
    let ok = constr "ok" sexp (fun x -> Ok x) in
    let error = constr "error" Error.sexp (fun x -> Error x) in
    sum
      [ econstr ok; econstr error ]
      (function
        | Ok s -> case s ok
        | Error e -> case e error)
  ;;

  let fields =
    let open Conv in
    let id = Id.required_field in
    let payload = field "result" (required result) in
    both id payload
  ;;

  let to_dyn = Result.to_dyn Sexp.to_dyn Error.to_dyn
end

module Protocol = struct
  type t = int

  let latest_version = 0
  let sexp = Conv.int
end

module Initialize = struct
  module Request = struct
    type t =
      { dune_version : Version.t
      ; protocol_version : Protocol.t
      ; id : Id.t
      }

    let dune_version t = t.dune_version
    let protocol_version t = t.protocol_version
    let id t = t.id

    let create ~id =
      let dune_version = Version.latest in
      let protocol_version = Protocol.latest_version in
      { dune_version; protocol_version; id }
    ;;

    let method_name = "initialize"

    let sexp =
      let open Conv in
      let dune_version = field "dune_version" (required Version.sexp) in
      let protocol_version = field "protocol_version" (required Protocol.sexp) in
      let id = Id.required_field in
      let to_ (dune_version, protocol_version, id) =
        { dune_version; protocol_version; id }
      in
      let from { dune_version; protocol_version; id } =
        dune_version, protocol_version, id
      in
      record (iso (three dune_version protocol_version id) to_ from)
    ;;

    let of_call { Call.method_; params } ~version =
      if String.equal method_ method_name
      then Conv.of_sexp sexp ~version params |> Result.map_error ~f:Response.Error.of_conv
      else (
        let message = "initialize request expected" in
        Error (Response.Error.create ~message ~kind:Invalid_request ()))
    ;;

    let to_call t =
      let params = Conv.to_sexp sexp t in
      { Call.method_ = "initialize"; params }
    ;;
  end

  module Response = struct
    type t = unit

    let sexp = Conv.unit
    let create () = ()
    let to_response t = Conv.to_sexp sexp t
  end
end

module Version_negotiation = struct
  module Request = struct
    type t = Menu of (Method.Name.t * Method.Version.t list) list

    let method_name = "version_menu"
    let create menu = Menu menu

    let sexp =
      Conv.(
        iso
          (list (pair Method.Name.sexp (list Method.Version.sexp)))
          (fun x -> Menu x)
          (function
            | Menu x -> x))
    ;;

    let to_call t =
      let params = Conv.to_sexp sexp t in
      { Call.method_ = method_name; params }
    ;;

    let of_call { Call.method_; params } ~version =
      if String.equal method_ method_name
      then Conv.of_sexp sexp ~version params |> Result.map_error ~f:Response.Error.of_conv
      else (
        let message = "version negotiation request expected" in
        Error (Response.Error.create ~message ~kind:Invalid_request ()))
    ;;
  end

  module Response = struct
    type t = Selected of (Method.Name.t * Method.Version.t) list

    let sexp =
      Conv.(
        iso
          (list (pair Method.Name.sexp Method.Version.sexp))
          (fun x -> Selected x)
          (function
            | Selected x -> x))
    ;;

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
      sum
        [ econstr packet; econstr close_connection ]
        (function
          | Packet p -> case p packet
          | Close_connection -> case () close_connection)
    ;;
  end

  module In = struct
    type t =
      | New_connection
      | Packet of Csexp.t
      | Close_connection

    let to_dyn =
      let open Dyn in
      function
      | New_connection -> variant "New_connection" []
      | Close_connection -> variant "Close_connection" []
      | Packet sexp -> variant "Packet" [ Sexp.to_dyn sexp ]
    ;;

    let sexp =
      let open Conv in
      let new_connection = constr "new_connection" unit (fun () -> New_connection) in
      let packet = constr "packet" sexp (fun p -> Packet p) in
      let close_connection =
        constr "close_connection" unit (fun () -> Close_connection)
      in
      sum
        [ econstr new_connection; econstr packet; econstr close_connection ]
        (function
          | New_connection -> case () new_connection
          | Packet p -> case p packet
          | Close_connection -> case () close_connection)
    ;;
  end
end

module Packet = struct
  type t =
    | Request of Request.t
    | Response of (Id.t * Response.t)
    | Notification of Call.t

  let sexp =
    let open Conv in
    let to_ (id, result, call) =
      match id, result, call with
      | Some id, None, Some call -> Request (id, call)
      | None, None, Some call -> Notification call
      | Some id, Some result, None -> Response (id, result)
      | _, _, _ -> Conv.error (Parse_error { message = "invalid packet"; payload = [] })
    in
    let from = function
      | Request (id, payload) -> Some id, None, Some payload
      | Response (id, response) -> Some id, Some response, None
      | Notification call -> None, None, Some call
    in
    record
    @@ iso
         (let id = Id.optional_field in
          let result = field "result" (optional Response.result) in
          let method_ = field "method" (optional Method.Name.sexp) in
          let params = field "params" (optional sexp) in
          let call =
            iso
              (both method_ params)
              (fun (method_, params) ->
                match method_, params with
                | Some method_, Some params -> Some { Call.method_; params }
                | None, None -> None
                | Some _, None | None, Some _ ->
                  Conv.error (Parse_error { message = "invalid call"; payload = [] }))
              (function
                | Some { Call.method_; params } -> Some method_, Some params
                | None -> None, None)
          in
          three id result call)
         to_
         from
  ;;
end

module Decl = struct
  type 'gen t =
    { method_ : Method.Name.t
    ; key : 'gen Method.Version.Map.t Univ_map.Key.t
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
    type ('req, 'resp) gen = Method.Version.t * ('req, 'resp) Generation.t

    let make_gen
      ~req
      ~resp
      ~upgrade_req
      ~downgrade_req
      ~upgrade_resp
      ~downgrade_resp
      ~version
      =
      ( version
      , Generation.T
          { req; resp; upgrade_req; downgrade_req; upgrade_resp; downgrade_resp } )
    ;;

    let make_current_gen ~req ~resp ~version =
      make_gen
        ~req
        ~resp
        ~upgrade_req:Fun.id
        ~downgrade_req:Fun.id
        ~upgrade_resp:Fun.id
        ~downgrade_resp:Fun.id
        ~version
    ;;

    let gen_to_dyn _ = Dyn.String "<generation>"

    type ('req, 'resp) witness = ('req, 'resp) Generation.t t

    type nonrec ('req, 'resp) t =
      { decl : ('req, 'resp) witness
      ; generations : ('req, 'resp) gen list
      }

    let make ~method_ ~generations =
      { generations
      ; decl =
          { method_; key = Univ_map.Key.create ~name:method_ (Int.Map.to_dyn gen_to_dyn) }
      }
    ;;

    let print_generation_list ~include_response generations =
      List.iter generations ~f:(fun (version, Generation.T conv) ->
        let conv_to_digest conv =
          let sexp_string = Sexp.to_string (Conv.sexp_for_digest conv) in
          if String.length sexp_string < 32
          then sexp_string
          else Digest.to_hex (Digest.string sexp_string)
        in
        let req = conv_to_digest conv.req in
        let resp = conv_to_digest conv.resp in
        if include_response
        then Printf.printf "Version %d:\n  Request: %s\n  Response: %s\n" version req resp
        else Printf.printf "Version %d: %s\n" version req)
    ;;

    let print_generations t = print_generation_list ~include_response:true t.generations
    let witness t = t.decl
  end

  module Notification = struct
    type 'payload gen = Method.Version.t * ('payload, unit) Generation.t

    let make_gen
      (type a b)
      ~(conv : a Conv.value)
      ~(upgrade : a -> b)
      ~(downgrade : b -> a)
      ~version
      : b gen
      =
      ( version
      , Generation.T
          { req = conv
          ; resp = Conv.unit
          ; upgrade_req = upgrade
          ; downgrade_req = downgrade
          ; upgrade_resp = Fun.id
          ; downgrade_resp = Fun.id
          } )
    ;;

    let make_current_gen (type a) ~(conv : a Conv.value) ~version : a gen =
      make_gen ~conv ~upgrade:Fun.id ~downgrade:Fun.id ~version
    ;;

    let gen_to_dyn _ = Dyn.String "<generation>"

    type 'payload witness = ('payload, unit) Generation.t t

    type nonrec 'payload t =
      { decl : 'payload witness
      ; generations : 'payload gen list
      }

    let make ~method_ ~generations =
      { generations
      ; decl =
          { method_; key = Univ_map.Key.create ~name:method_ (Int.Map.to_dyn gen_to_dyn) }
      }
    ;;

    let print_generations t =
      Request.print_generation_list ~include_response:false t.generations
    ;;

    let witness t = t.decl
  end

  type ('a, 'b) request = ('a, 'b) Request.t
  type 'a notification = 'a Notification.t
end
