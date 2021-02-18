open Stdune

module Conv : module type of Conv

module Id : sig
  type t

  val sexp : t Conv.value

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val make : Sexp.t -> t

  module Set : Set.S with type elt = t
end

module Call : sig
  type t =
    { method_ : string
    ; params : Sexp.t
    }

  val create : ?params:Sexp.t -> method_:string -> unit -> t
end

module Request : sig
  type t = Id.t * Call.t
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

    exception E of t

    val of_conv : Conv.error -> t

    val create : ?payload:Sexp.t -> kind:kind -> message:string -> unit -> t
  end

  type t = (Sexp.t, Error.t) result
end

module Initialize : sig
  module Request : sig
    type t =
      { version : int * int
      ; id : Id.t
      }

    val create : id:Id.t -> t

    val version : t -> int * int

    val id : t -> Id.t

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result
  end

  module Response : sig
    type t

    val create : unit -> t

    val to_response : t -> Stdune.Sexp.t

    val sexp : t Conv.value
  end
end

module Decl : sig
  type ('req, 'resp) request =
    { method_ : string
    ; req : 'req Conv.value
    ; resp : 'resp Conv.value
    }

  type 'req notification =
    { method_ : string
    ; req : 'req Conv.value
    }

  val notification : method_:string -> 'a Conv.value -> 'a notification

  val request :
    method_:string -> 'a Conv.value -> 'b Conv.value -> ('a, 'b) request
end

module Where : sig
  type t =
    [ `Unix of Path.t
    | `Ip of Unix.inet_addr * [ `Port of int ]
    ]

  val get : unit -> t option

  val to_string : t -> string

  val default : unit -> t

  val add_to_env : t -> Env.t -> Env.t
end

module Loc : sig
  type t =
    { start : Lexing.position
    ; stop : Lexing.position
    }
end

module Error : sig
  type t =
    { target : string option
    ; message : string
    ; loc : Loc.t option
    }
end

module Log : sig
  type t =
    { payload : Sexp.t option
    ; message : string
    }
end

module Promotion : sig
  type t =
    { in_build : string
    ; in_source : string
    }
end

module Subscribe : sig
  type t =
    | Error
    | Promotion
end

module Client (S : sig
  module Fiber : sig
    type 'a t

    val return : 'a -> 'a t

    val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

    val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

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

  module Chan : sig
    type t

    val write : t -> Sexp.t option -> unit Fiber.t

    val read : t -> Sexp.t option Fiber.t
  end
end) : sig
  open S

  type t

  val request :
       ?id:Id.t
    -> t
    -> ('a, 'b) Decl.request
    -> 'a
    -> ('b, Response.Error.t) result Fiber.t

  val notification : t -> 'a Decl.notification -> 'a -> unit Fiber.t

  module Handler : sig
    type t

    val create :
         ?log:(Log.t -> unit Fiber.t)
      -> ?errors:(Error.t list -> unit Fiber.t)
      -> ?promotions:(Promotion.t list -> unit Fiber.t)
      -> unit
      -> t
  end

  val connect_raw :
       Chan.t
    -> Initialize.Request.t
    -> on_notification:(Call.t -> unit Fiber.t)
    -> f:(t -> 'a Fiber.t)
    -> 'a Fiber.t

  val connect :
       ?handler:Handler.t
    -> Chan.t
    -> Initialize.Request.t
    -> f:(t -> 'a Fiber.t)
    -> 'a Fiber.t
end

module Message : sig
  type t =
    | Request of Request.t
    | Notification of Call.t

  val sexp : t Conv.value
end

module Packet : sig
  type t =
    | Response of (Id.t * Response.t)
    | Notification of Call.t

  val sexp : t Conv.value
end

module Version : sig
  type t = int * int

  val latest : t

  val sexp : t Conv.value
end

module Public : sig
  (** Public requests and notifications *)

  module Request : sig
    type ('a, 'b) t = ('a, 'b) Decl.request
  end

  module Notification : sig
    type 'a t = 'a Decl.notification

    val subscribe : Subscribe.t t

    val unsubscribe : Subscribe.t t

    val shutdown : unit t
  end
end

module Server_notifications : sig
  (** Notification sent from server to client *)

  val errors : Error.t list Decl.notification

  val promotions : Promotion.t list Decl.notification

  val log : Log.t Decl.notification
end
