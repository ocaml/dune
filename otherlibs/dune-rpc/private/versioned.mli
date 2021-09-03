(** The main logic for the runtime versioning protocol for the Dune RPC. For a
    high-level explanation and rationale, see [doc/dev/rpc-versioning.ml]. *)

open Stdune
open Types

module Menu : sig
  type t

  val default : t

  (** For each method known by both local and remote, choose the highest common
      version number. Returns [None] if the resulting menu would be empty. *)
  val select_common :
       local_versions:Int.Set.t String.Map.t
    -> remote_versions:(string * int list) list
    -> t option

  val of_list : (string * int) list -> (t, string * int * int) result

  val to_list : t -> (string * int) list

  val to_dyn : t -> Dyn.t
end

module Make (Fiber : Fiber) : sig
  module Handler : sig
    type 'state t

    val handle_request : 'state t -> 'state -> Request.t -> Response.t Fiber.t

    val handle_notification :
      'state t -> 'state -> Call.t -> (unit, Response.Error.t) result Fiber.t

    val prepare_request :
         'a t
      -> ('req, 'resp) Decl.Request.witness
      -> 'req
      -> ( Call.t * (Csexp.t -> ('resp, Response.Error.t) result)
         , Response.Error.t )
         result

    val prepare_notification :
         'a t
      -> 'payload Decl.Notification.witness
      -> 'payload
      -> (Call.t, Response.Error.t) result
  end

  module Builder : sig
    type 'state t

    val to_handler :
         'state t
      -> session_version:('state -> int * int)
      -> menu:Menu.t
      -> 'state Handler.t

    val create : unit -> 'state t

    val registered_procedures : 'a t -> (string * int list) list

    (** A *declaration* of a procedure is a claim that this side of the session
        is able to *initiate* that procedure. Correspondingly, *implementing* a
        procedure enables you to *receive* that procedure (and probably do
        something in response).

        Currently, attempting to both implement and declare the same procedure
        in the same builder will raise. While there is nothing fundamentally
        wrong with allowing this, it is simpler for the initial version
        negotiation to treat all method names uniformly, rather than specifying
        whether a given (set of) generation(s) is implemented or declared.

        Finally, attempting to declare or implement the same generation twice
        will also raise. *)
    val declare_notification : 'state t -> 'payload Decl.notification -> unit

    val declare_request : 'state t -> ('req, 'resp) Decl.request -> unit

    val implement_notification :
         'state t
      -> 'payload Decl.notification
      -> ('state -> 'payload -> unit Fiber.t)
      -> unit

    val implement_request :
         'state t
      -> ('req, 'resp) Decl.request
      -> ('state -> 'req -> 'resp Fiber.t)
      -> unit
  end
end
