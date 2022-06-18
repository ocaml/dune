open Import

module Id : sig
  type t

  val make : Sexp.t -> t

  val sexp : (t, Conv.values) Conv.t

  val required_field : (t, Conv.fields) Conv.t

  val optional_field : (t option, Conv.fields) Conv.t

  val to_dyn : t -> Dyn.t

  val hash : t -> int

  val equal : t -> t -> bool

  val to_sexp : t -> Sexp.t

  include Stdune.Comparable_intf.S with type key := t
end

module Version : sig
  type t = int * int

  val latest : t

  val sexp : t Conv.value
end

module Method_name : sig
  type t = string

  val sexp : t Conv.value

  module Map = Stdune.String.Map
  module Table = Stdune.String.Table
end

module Method_version : sig
  type t = int

  val sexp : t Conv.value

  module Set = Stdune.Int.Set
  module Map = Stdune.Int.Map
end

module Call : sig
  (** Represents a single rpc call. Request or notification. *)

  type t =
    { method_ : Method_name.t
    ; params : Sexp.t
    }

  val to_dyn : t -> Dyn.t

  val create : ?params:Sexp.t -> method_:Method_name.t -> unit -> t

  val fields : (t, Conv.fields) Conv.t
end

module Response : sig
  module Error : sig
    type kind =
      | Invalid_request
      | Code_error

    type t =
      { payload : Sexp.t option
      ; message : string
      ; kind : kind
      }

    val to_dyn : t -> Dyn.t

    val payload : t -> Sexp.t option

    val message : t -> string

    val kind : t -> kind

    exception E of t

    val create : ?payload:Sexp.t -> kind:kind -> message:string -> unit -> t

    val of_conv : Conv.error -> t
  end

  type t = (Sexp.t, Error.t) result

  val fields : (Id.t * t, Conv.fields) Conv.t

  val to_dyn : t -> Dyn.t
end

module Request : sig
  type t = Id.t * Call.t
end

module Protocol : sig
  type t = int

  val latest_version : t

  val sexp : t Conv.value
end

module Initialize : sig
  module Request : sig
    type t =
      { dune_version : int * int
      ; protocol_version : int
      ; id : Id.t
      }

    val create : id:Id.t -> t

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result

    val dune_version : t -> int * int

    val protocol_version : t -> int

    val id : t -> Id.t

    val to_call : t -> Call.t
  end

  module Response : sig
    type t

    val create : unit -> t

    val to_response : t -> Sexp.t

    val sexp : t Conv.value
  end
end

module Version_negotiation : sig
  module Request : sig
    type t = private Menu of (string * int list) list

    val create : (string * int list) list -> t

    val sexp : t Conv.value

    val to_call : t -> Call.t

    val of_call : Call.t -> version:Version.t -> (t, Response.Error.t) result
  end

  module Response : sig
    type t = private Selected of (string * int) list

    val create : (string * int) list -> t

    val to_response : t -> Sexp.t

    val sexp : t Conv.value
  end
end

module Persistent : sig
  module In : sig
    type t =
      | New_connection
      | Packet of Csexp.t
      | Close_connection

    val sexp : t Conv.value

    val to_dyn : t -> Dyn.t
  end

  module Out : sig
    type t =
      | Packet of Sexp.t
      | Close_connection

    val sexp : t Conv.value
  end
end

module Packet : sig
  module Query : sig
    type t =
      | Request of Request.t
      | Notification of Call.t

    val sexp : t Conv.value
  end

  module Reply : sig
    type t =
      | Response of (Id.t * Response.t)
      | Notification of Call.t

    val sexp : t Conv.value
  end
end

module Decl : sig
  type 'gen t =
    { method_ : Method_name.t
    ; key : 'gen Int.Map.t Stdune.Univ_map.Key.t
    }

  module Generation : sig
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

  module Request : sig
    type ('req, 'resp) gen = Method_version.t * ('req, 'resp) Generation.t

    val make_gen :
         req:'wire_req Conv.value
      -> resp:'wire_resp Conv.value
      -> upgrade_req:('wire_req -> 'req)
      -> downgrade_req:('req -> 'wire_req)
      -> upgrade_resp:('wire_resp -> 'resp)
      -> downgrade_resp:('resp -> 'wire_resp)
      -> version:Method_version.t
      -> ('req, 'resp) gen

    val make_current_gen :
         req:'req Conv.value
      -> resp:'resp Conv.value
      -> version:Method_version.t
      -> ('req, 'resp) gen

    type ('req, 'resp) witness = ('req, 'resp) Generation.t t

    type nonrec ('req, 'resp) t =
      { decl : ('req, 'resp) witness
      ; generations : ('req, 'resp) gen list
      }

    val make :
         method_:Method_name.t
      -> generations:('req, 'resp) gen list
      -> ('req, 'resp) t

    val witness : ('a, 'b) t -> ('a, 'b) witness
  end

  module Notification : sig
    type 'payload gen = Method_version.t * ('payload, unit) Generation.t

    val make_gen :
         conv:'wire Conv.value
      -> upgrade:('wire -> 'model)
      -> downgrade:('model -> 'wire)
      -> version:Method_version.t
      -> 'model gen

    val make_current_gen :
      conv:'model Conv.value -> version:Method_version.t -> 'model gen

    type 'payload witness = ('payload, unit) Generation.t t

    type nonrec 'payload t =
      { decl : 'payload witness
      ; generations : 'payload gen list
      }

    val make :
      method_:Method_name.t -> generations:'payload gen list -> 'payload t

    val witness : 'a t -> 'a witness
  end

  type ('a, 'b) request = ('a, 'b) Request.t

  type 'a notification = 'a Notification.t
end
