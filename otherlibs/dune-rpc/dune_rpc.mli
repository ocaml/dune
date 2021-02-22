(** Implementation of the protocol used by dune rpc. Independent of IO and any
    specific rpc requests. The protocol described here is stable and is relied
    on by 3rd party clients.

    The implementation is loosely modelled on jsonrpc. It defines the following
    concepts:

    Session - An active rpc session

    Request - A unique id with a call sent by a client. A server must respond to
    every request

    Notification - A call send by a client. A server must not respond to a
    notification

    It contains hooks that make it possible to use with any custom scheduler
    that uses fibers *)

module V1 : sig
  module Sexp : sig
    type t =
      | Atom of string
      | List of t list
  end

  module Id : sig
    (** Id's for requests, responses, sessions.

        Id's are permitted to be arbtirary s-expressions to allow users pick
        descriptive tokens to ease debugging. *)

    type t

    val make : Sexp.t -> t
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
    end

    type t = (Sexp.t, Error.t) result
  end

  module Initialize : sig
    type t

    val create : id:Id.t -> t
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

  module Log : sig
    type t =
      { payload : Sexp.t option
      ; message : string
      }
  end

  module Notification : sig
    type 'a t

    val subscribe : Subscribe.t t

    val unsubscribe : Subscribe.t t

    val shutdown : unit t
  end

  module Request : sig
    type ('a, 'b) t
  end

  (** Functor to create a client implementation *)
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

      (* [write t x] writes the s-expression when [x] is [Some _], and closes
         the session if [x = None] *)
      val write : t -> Sexp.t option -> unit Fiber.t

      (* [read t] attempts to read from [t]. If an s-expression is read, it is
         returned as [Some sexp], otherwise [None] is returned and the session
         is closed. *)
      val read : t -> Sexp.t option Fiber.t
    end
  end) : sig
    open S

    (** Rpc client *)

    type t

    module Handler : sig
      type t

      val create :
           ?log:(Log.t -> unit Fiber.t)
        -> ?errors:(Error.t list -> unit Fiber.t)
        -> ?promotions:(Promotion.t list -> unit Fiber.t)
        -> unit
        -> t
    end

    (** [request ?id client decl req] send a request [req] specified by [decl]
        to [client]. If [id] is [None], it will be automatically generated. *)
    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Request.t
      -> 'a
      -> ('b, Response.Error.t) result Fiber.t

    val notification : t -> 'a Notification.t -> 'a -> unit Fiber.t

    (** [connect ?on_handler session init ~f] connect to [session], initialize
        with [init] and call [f] once the client is initialized. [handler] is
        called for some notifications sent to [session] *)
    val connect :
         ?handler:Handler.t
      -> Chan.t
      -> Initialize.t
      -> f:(t -> 'a Fiber.t)
      -> 'a Fiber.t
  end
end
