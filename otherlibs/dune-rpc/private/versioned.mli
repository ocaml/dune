(** The main logic for the runtime versioning protocol for the Dune RPC. For a
    high-level explanation and rationale, see [doc/dev/rpc-versioning.ml]. *)

open Types

module Version_error : sig
  type t

  val payload : t -> Csexp.t option

  val message : t -> string

  exception E of t
end

module Staged : sig
  type ('req, 'resp) request =
    { encode_req : 'req -> Call.t
    ; decode_resp : Csexp.t -> ('resp, Response.Error.t) result
    }

  type 'payload notification = { encode : 'payload -> Call.t }
end

module type S = sig
  type 'a fiber

  module Handler : sig
    type 'state t

    val handle_request : 'state t -> 'state -> Request.t -> Response.t fiber

    val handle_notification :
      'state t -> 'state -> Call.t -> (unit, Response.Error.t) result fiber

    val prepare_request :
         'a t
      -> ('req, 'resp) Decl.Request.witness
      -> (('req, 'resp) Staged.request, Version_error.t) result

    val prepare_notification :
         'a t
      -> 'payload Decl.Notification.witness
      -> ('payload Staged.notification, Version_error.t) result
  end

  module Builder : sig
    type 'state t

    val to_handler :
         'state t
      -> session_version:('state -> Version.t)
      -> menu:Menu.t
      -> 'state Handler.t

    val create : unit -> 'state t

    val registered_procedures :
      'a t -> (Method.Name.t * Method.Version.t list) list

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
      -> ('state -> 'payload -> unit fiber)
      -> unit

    val implement_request :
         'state t
      -> ('req, 'resp) Decl.request
      -> ('state -> 'req -> 'resp fiber)
      -> unit
  end
end

module Make (Fiber : Fiber_intf.S) : S with type 'a fiber := 'a Fiber.t
