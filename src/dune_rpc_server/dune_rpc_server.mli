open Stdune
open Dune_rpc_private

module Poller : sig
  type t

  val to_dyn : t -> Dyn.t

  val compare : t -> t -> Ordering.t

  val name : t -> Procedures.Poll.Name.t
end

module Session : sig
  module Id : Stdune.Id.S

  (** Session representing a connected client with a custom state. *)
  type 'a t

  val id : _ t -> Id.t

  (* [initialize session] returns the initialize request used to initialize this
     session *)
  val initialize : _ t -> Initialize.Request.t

  (** [get session] returns the current session state. It is an error to access
      the state after [on_terminate] finishes. *)
  val get : 'a t -> 'a

  (** [get session a] sets the current state to [a].*)
  val set : 'a t -> 'a -> unit

  (** [notification session n a] Send notification [a] defined by [n] to
      [session] *)
  val notification : _ t -> 'a Decl.Notification.witness -> 'a -> unit Fiber.t

  val compare : 'a t -> 'a t -> Ordering.t

  val request_close : 'a t -> unit Fiber.t

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val has_poller : _ t -> Poller.t -> bool

  val closed : _ t -> unit Fiber.t

  (** A ['a Session.Stage1.t] represents a session prior to version negotiation.

      Used during initialization. *)
  module Stage1 : sig
    type 'a t

    val id : _ t -> Id.t

    val initialize : _ t -> Initialize.Request.t

    val get : 'a t -> 'a

    val set : 'a t -> 'a -> unit

    val compare : 'a t -> 'a t -> Ordering.t

    val request_close : 'a t -> unit Fiber.t

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

    (** Register a callback to be called once version negotiation has concluded.
        At most one callback can be set at once; calling this function multiple
        times for the same session will override previous invocations.

        The registered callback is guaranteed to be called at most once. *)
    val register_upgrade_callback : _ t -> (Menu.t -> unit) -> unit
  end
end

module Handler : sig
  (** A handler defines everything necessary to serve a session. It contains

      - A way to defined requests and notifications
      - A hook for handling session initiation/termination *)

  type 'a t

  val create :
       ?on_terminate:('a Session.t -> unit Fiber.t)
         (** Termination hook. It is guaranteed that the session state will not
             modified after this function is called *)
    -> on_init:('a Session.Stage1.t -> Initialize.Request.t -> 'a Fiber.t)
         (** Initiation hook. It's guaranteed to be called before any
             requests/notifications. It's job is to initialize the session
             state. *)
    -> version:int * int
         (** version of the rpc. it's expected to support all earlier versions *)
    -> unit
    -> 'a t

  (** [implement_request handler decl callback] Add a request to [handler] using
      [callback] as the implementation and [decl] as the metadata *)
  val implement_request :
    's t -> ('a, 'b) Decl.request -> ('s Session.t -> 'a -> 'b Fiber.t) -> unit

  (** [implement_notification handler decl callback] Add a notification to
      [handler] using [callback] as the implementation and [decl] as the
      metadata *)
  val implement_notification :
    's t -> 'a Decl.notification -> ('s Session.t -> 'a -> unit Fiber.t) -> unit

  (** [declare_notification handler decl] Declares that this server may send
      notifications according to metadata [decl]. *)
  val declare_notification : 's t -> 'a Decl.notification -> unit

  val implement_long_poll :
       _ t
    -> 'diff Procedures.Poll.t
    -> 'state Fiber.Svar.t
    -> equal:('state -> 'state -> bool)
    -> diff:(last:'state option -> now:'state -> 'diff)
    -> unit

  module Private : sig
    val implement_poll :
         's t
      -> 'a Procedures.Poll.t
      -> on_poll:('s Session.t -> Poller.t -> 'a option Fiber.t)
      -> on_cancel:('s Session.t -> Poller.t -> unit Fiber.t)
      -> unit
  end
end

type t

val make : 'a Handler.t -> t

(** Functor to create a server implementation *)
module Make (S : sig
  type t

  (* [write t x] writes the s-expression when [x] is [Some _], and closes the
     session if [x = None] *)
  val write : t -> Sexp.t list option -> unit Fiber.t

  (* [read t] attempts to read from [t]. If an s-expression is read, it is
     returned as [Some sexp], otherwise [None] is returned and the session is
     closed. *)
  val read : t -> Sexp.t option Fiber.t
end) : sig
  (** [serve sessions handler] serve all [sessions] using [handler] *)
  val serve : S.t Fiber.Stream.In.t -> Dune_stats.t option -> t -> unit Fiber.t
end
