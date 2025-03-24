open Stdune
open Dune_rpc_private

module Session : sig
  module Id : Stdune.Id.S

  (** Session representing a connected client with a custom state. *)
  type 'a t

  val id : _ t -> Id.t

  (** Name of the endpoint the session is connected to. *)
  val name : _ t -> string

  (** [get session] returns the current session state. It is an error to access
      the state after [on_terminate] finishes. *)
  val get : 'a t -> 'a

  (** [get session a] sets the current state to [a].*)
  val set : 'a t -> 'a -> unit

  (** [prepare t n] prepares a notification [n] by checking if [t] supports it.
      If it supports it, [Ok _] is returned. Otherwise [Error _] is returned. *)
  val prepare_notification
    :  _ t
    -> 'payload Decl.Notification.witness
    -> ('payload Versioned.Staged.notification, Version_error.t) result

  (** [send_notification session n a] Send notification [a] defined by [n] to
      [session] *)
  val send_notification : _ t -> 'a Versioned.Staged.notification -> 'a -> unit Fiber.t

  (** [request t r id payload] sends a request [r] to [t] with [id] and
      [payload].

      Note that any request must be declared with [declare_request] before
      sending it *)
  val request
    :  _ t
    -> ('a, 'b) Decl.Request.witness
    -> Dune_rpc_private.Id.t
    -> 'a
    -> 'b Fiber.t

  val compare : 'a t -> 'a t -> Ordering.t
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  val closed : _ t -> unit Fiber.t

  (** A ['a Session.Stage1.t] represents a session prior to version negotiation.

      Used during initialization. *)
  module Stage1 : sig
    type 'a t

    val id : _ t -> Id.t
    val menu : _ t -> Menu.t option

    (* [initialize session] returns the initialize request used to initialize this
       session *)
    val initialize : _ t -> Initialize.Request.t
    val close : 'a t -> unit Fiber.t
    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
    val name : _ t -> string
  end
end

module Handler : sig
  (** A handler defines everything necessary to serve a session. It contains

      - A way to defined requests and notifications
      - A hook for handling session initiation/termination *)

  type 'a t

  val create
    :  ?on_terminate:('a Session.Stage1.t -> unit Fiber.t)
         (** Termination hook. It is guaranteed that the session state will not
             modified after this function is called *)
    -> on_init:('a Session.Stage1.t -> Initialize.Request.t -> 'a Fiber.t)
         (** Initiation hook. It's guaranteed to be called before any
             requests/notifications. It's job is to initialize the session
             state. *)
    -> ?on_upgrade:('a Session.t -> Menu.t -> unit Fiber.t)
         (** called immediately after the client has finished negotiation *)
    -> version:int * int
         (** version of the rpc. it's expected to support all earlier versions *)
    -> unit
    -> 'a t

  (** [implement_request handler decl callback] Add a request to [handler] using
      [callback] as the implementation and [decl] as the metadata *)
  val implement_request
    :  's t
    -> ('a, 'b) Decl.request
    -> ('s Session.t -> 'a -> 'b Fiber.t)
    -> unit

  (** [implement_notification handler decl callback] Add a notification to
      [handler] using [callback] as the implementation and [decl] as the
      metadata *)
  val implement_notification
    :  's t
    -> 'a Decl.notification
    -> ('s Session.t -> 'a -> unit Fiber.t)
    -> unit

  (** [declare_notification handler decl] Declares that this server may send
      notifications according to metadata [decl]. *)
  val declare_notification : 's t -> 'a Decl.notification -> unit

  (** [declare_request handle decl] declares that this server may send requests
      according to the metadata [decl]. *)
  val declare_request : 's t -> ('a, 'b) Decl.request -> unit

  (** [implement_long_poll t proc_diff svar ~equal ~diff] will implement long
      polling routines to sync the state variable [svar]. [equal] is used to
      prevent updates that do not modify the state. [diff] is used to generate
      efficient operations to bring the state up to date. *)
  val implement_long_poll
    :  _ t
    -> 'diff Procedures.Poll.t
    -> 'state Fiber.Svar.t
    -> equal:('state -> 'state -> bool)
    -> diff:(last:'state option -> now:'state -> 'diff)
    -> unit

  module For_tests : sig
    (** Raw long polling machinery only good for tests *)

    val implement_poll
      :  's t
      -> 'a Procedures.Poll.t
      -> on_poll:('s Session.t -> 'a option Fiber.t)
      -> on_cancel:('s Session.t -> unit Fiber.t)
      -> unit
  end
end

type t

val make : 'a Handler.t -> t

(** Functor to create a server implementation *)
module Make (S : sig
    type t

    (** [close t] closes the session *)
    val close : t -> unit Fiber.t

    (** [write t x] writes the s-expression *)
    val write : t -> Sexp.t list -> (unit, [ `Closed ]) result Fiber.t

    (** [read t] attempts to read from [t]. If an s-expression is read, it is
        returned as [Some sexp], otherwise [None] is returned and the session is
        closed. *)
    val read : t -> Sexp.t option Fiber.t

    (* [name t] returns the name of the endpoint the session is connected to. *)
    val name : t -> string
  end) : sig
  (** [serve sessions handler] serve all [sessions] using [handler] *)
  val serve : S.t Fiber.Stream.In.t -> Dune_stats.t option -> t -> unit Fiber.t
end
