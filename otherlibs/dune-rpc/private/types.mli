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

  module Set : Set.S with type elt = t
end

module Version : sig
  type t = int * int

  val latest : t

  val sexp : t Conv.value
end

module Call : sig
  (** Represents a single rpc call. Request or notification. *)

  type t =
    { method_ : string
    ; params : Sexp.t
    }

  val to_dyn : t -> Dyn.t

  val create : ?params:Sexp.t -> method_:string -> unit -> t

  val fields : (t, Conv.fields) Conv.t
end

module Response : sig
  module Error : sig
    type kind =
      | Invalid_request
      | Code_error
      | Version_error

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

module Initialize : sig
  module Request : sig
    type t =
      { version : int * int
      ; id : Id.t
      }

    val create : id:Id.t -> t

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result

    val version : t -> int * int

    val id : t -> Id.t

    val to_call : t -> Call.t
  end

  module Response : sig
    type t = private
      (* The [Compatibility] variant is for compatibility with older versions of
         Dune. If we receive it, we can then initiate version negotiation. *)
      | Compatibility
      | Supports_versioning of unit

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
  module Generation : sig
    type ('wire_req, 'wire_resp, 'real_req, 'real_resp) conv = private
      { req : 'wire_req Conv.value
      ; resp : 'wire_resp Conv.value
      ; upgrade_req : 'wire_req -> 'real_req
      ; downgrade_req : 'real_req -> 'wire_req
      ; upgrade_resp : 'wire_resp -> 'real_resp
      ; downgrade_resp : 'real_resp -> 'wire_resp
      }

    type (_, _) t = private
      | T :
          ('wire_req, 'wire_resp, 'real_req, 'real_resp) conv
          -> ('real_req, 'real_resp) t

    val current_request : 'req Conv.value -> 'resp Conv.value -> ('req, 'resp) t

    val prior_request :
         'wire_req Conv.value
      -> 'wire_resp Conv.value
      -> upgrade_req:('wire_req -> 'real_req)
      -> downgrade_req:('real_req -> 'wire_req)
      -> upgrade_resp:('wire_resp -> 'real_resp)
      -> downgrade_resp:('real_resp -> 'wire_resp)
      -> ('real_req, 'real_resp) t
  end

  module Generations : sig
    type ('req, 'resp) t

    val lookup : ('req, 'resp) t -> int -> ('req, 'resp) Generation.t option

    val iter :
      ('req, 'resp) t -> f:(int -> ('req, 'resp) Generation.t -> unit) -> unit
  end

  type ('req, 'resp) request =
    { method_ : string
    ; generations : ('req, 'resp) Generations.t
    }

  type 'req notification =
    { method_ : string
    ; req : 'req Conv.value
    }

  val notification : method_:string -> 'a Conv.value -> 'a notification

  val request :
       method_:string
    -> (int * ('req, 'resp) Generation.t) list
    -> ('req, 'resp) request
end
