module Conv : module type of Conv

module Where : module type of Where

module Registry : module type of Registry

module type Fiber = Fiber_intf.S

include module type of Exported_types

module Id : sig
  type t

  val sexp : t Conv.value

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val make : Csexp.t -> t

  val to_sexp : t -> Csexp.t

  include Stdune.Comparable_intf.S with type key := t
end

module Call : sig
  type t =
    { method_ : string
    ; params : Csexp.t
    }

  val create : ?params:Csexp.t -> method_:string -> unit -> t
end

module Version_error : sig
  type t

  val payload : t -> Csexp.t option

  val message : t -> string

  exception E of t
end

module Request : sig
  type t = Id.t * Call.t
end

module Response : sig
  module Error : sig
    type kind =
      | Invalid_request
      | Code_error

    type t =
      { payload : Csexp.t option
      ; message : string
      ; kind : kind
      }

    val payload : t -> Csexp.t option

    val message : t -> string

    val kind : t -> kind

    exception E of t

    val to_dyn : t -> Dyn.t

    val of_conv : Conv.error -> t

    val create : ?payload:Csexp.t -> kind:kind -> message:string -> unit -> t
  end

  type t = (Csexp.t, Error.t) result
end

module Initialize : sig
  module Request : sig
    type t =
      { dune_version : int * int
      ; protocol_version : int
      ; id : Id.t
      }

    val create : id:Id.t -> t

    val dune_version : t -> int * int

    val protocol_version : t -> int

    val id : t -> Id.t

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result
  end

  module Response : sig
    type t

    val create : unit -> t

    val to_response : t -> Csexp.t

    val sexp : t Conv.value
  end
end

module Version_negotiation : sig
  module Request : sig
    type t = private Menu of (string * int list) list

    val create : (string * int list) list -> t

    val sexp : t Conv.value

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result
  end

  module Response : sig
    type t

    val create : (string * int) list -> t

    val sexp : t Conv.value
  end
end

module Decl : sig
  module Request : sig
    type ('req, 'resp) gen

    val make_gen :
         req:'wire_req Conv.value
      -> resp:'wire_resp Conv.value
      -> upgrade_req:('wire_req -> 'req)
      -> downgrade_req:('req -> 'wire_req)
      -> upgrade_resp:('wire_resp -> 'resp)
      -> downgrade_resp:('resp -> 'wire_resp)
      -> version:int
      -> ('req, 'resp) gen

    val make_current_gen :
         req:'req Conv.value
      -> resp:'resp Conv.value
      -> version:int
      -> ('req, 'resp) gen

    type ('a, 'b) t

    val make :
      method_:string -> generations:('req, 'resp) gen list -> ('req, 'resp) t

    type ('a, 'b) witness

    val witness : ('a, 'b) t -> ('a, 'b) witness
  end

  module Notification : sig
    type 'payload gen

    val make_gen :
         conv:'wire Conv.value
      -> upgrade:('wire -> 'model)
      -> downgrade:('model -> 'wire)
      -> version:int
      -> 'model gen

    val make_current_gen : conv:'a Conv.value -> version:int -> 'a gen

    type 'a t

    val make : method_:string -> generations:'payload gen list -> 'payload t

    type 'a witness

    val witness : 'a t -> 'a witness
  end

  type ('a, 'b) request = ('a, 'b) Request.t

  type 'a notification = 'a Notification.t
end

module Procedures : sig
  (** Procedures with generations for server impl *)
  module Public : sig
    val ping : (unit, unit) Decl.Request.t

    val diagnostics : (unit, Diagnostic.t list) Decl.Request.t

    val shutdown : unit Decl.Notification.t

    val format_dune_file :
      (Path.t * [ `Contents of string ], string) Decl.Request.t

    val promote : (Path.t, unit) Decl.Request.t

    val build_dir : (unit, Path.t) Decl.Request.t
  end

  module Server_side : sig
    val abort : Message.t Decl.Notification.t

    val log : Message.t Decl.Notification.t
  end

  module Poll : sig
    type 'a t

    val poll : 'a t -> (Id.t, 'a option) Decl.Request.t

    val cancel : 'a t -> Id.t Decl.Notification.t

    module Name : sig
      type t

      val make : string -> t

      val compare : t -> t -> Ordering.t
    end

    val name : 'a t -> Name.t

    val make : Name.t -> (Id.t, 'a option) Decl.Request.gen list -> 'a t

    val progress : Progress.t t

    val diagnostic : Diagnostic.Event.t list t
  end
end

module Sub : sig
  type 'a t

  val of_procedure : 'a Procedures.Poll.t -> 'a t

  val poll : 'a t -> (Id.t, 'a option) Decl.Request.witness

  val poll_cancel : 'a t -> Id.t Decl.Notification.witness

  module Id : sig
    type t

    val compare : t -> t -> Ordering.t
  end

  val id : 'a t -> Id.t
end

module Public : sig
  (** Public requests and notifications *)
  module Request : sig
    type ('a, 'b) t = ('a, 'b) Decl.Request.witness

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t

    val format_dune_file : (Path.t * [ `Contents of string ], string) t

    val promote : (Path.t, unit) t

    val build_dir : (unit, Path.t) t
  end

  module Notification : sig
    type 'a t = 'a Decl.Notification.witness

    val shutdown : unit t
  end

  module Sub : sig
    type 'a t = 'a Sub.t

    val diagnostic : Diagnostic.Event.t list t

    val progress : Progress.t t
  end
end

module Client : sig
  module type S = sig
    type t

    type 'a fiber

    type chan

    module Versioned : sig
      type ('a, 'b) request = ('a, 'b) Versioned.Staged.request

      type 'a notification = 'a Versioned.Staged.notification

      val prepare_request :
           t
        -> ('a, 'b) Decl.Request.witness
        -> (('a, 'b) request, Version_error.t) result fiber

      val prepare_notification :
           t
        -> 'a Decl.Notification.witness
        -> ('a notification, Version_error.t) result fiber
    end

    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Versioned.request
      -> 'a
      -> ('b, Response.Error.t) result fiber

    val notification : t -> 'a Versioned.notification -> 'a -> unit fiber

    val disconnected : t -> unit fiber

    module Stream : sig
      type 'a t

      val cancel : _ t -> unit fiber

      val next : 'a t -> 'a option fiber
    end

    val poll :
      ?id:Id.t -> t -> 'a Sub.t -> ('a Stream.t, Version_error.t) result fiber

    module Batch : sig
      type t

      type client

      val create : client -> t

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Versioned.request
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Versioned.notification -> 'a -> unit

      val submit : t -> unit fiber
    end
    with type client := t

    module Handler : sig
      type t

      val create :
           ?log:(Message.t -> unit fiber)
        -> ?abort:(Message.t -> unit fiber)
        -> unit
        -> t
    end

    type proc =
      | Request : ('a, 'b) Decl.request -> proc
      | Notification : 'a Decl.notification -> proc
      | Poll : 'a Procedures.Poll.t -> proc

    val connect_with_menu :
         ?handler:Handler.t
      -> private_menu:proc list
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber

    val connect :
         ?handler:Handler.t
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber
  end

  module Make
      (Fiber : Fiber) (Chan : sig
        type t

        val write : t -> Csexp.t list option -> unit Fiber.t

        val read : t -> Csexp.t option Fiber.t
      end) : S with type 'a fiber := 'a Fiber.t and type chan := Chan.t
end

module Packet : sig
  module Reply : sig
    type t =
      | Response of (Id.t * Response.t)
      | Notification of Call.t

    val sexp : t Conv.value
  end

  module Query : sig
    type t =
      | Request of Request.t
      | Notification of Call.t

    val sexp : t Conv.value
  end
end

module Version : sig
  type t = int * int

  val latest : t

  val sexp : t Conv.value
end

module Protocol : sig
  type t = int

  val latest_version : t

  val sexp : t Conv.value
end

module Menu : sig
  type t

  val default : t

  (** For each method known by both local and remote, choose the highest common
      version number. Returns [None] if the resulting menu would be empty. *)
  val select_common :
       local_versions:Stdune.Int.Set.t Stdune.String.Map.t
    -> remote_versions:(string * int list) list
    -> t option

  val of_list : (string * int) list -> (t, string * int * int) result

  val to_list : t -> (string * int) list

  val to_dyn : t -> Dyn.t
end

module Versioned : sig
  module Staged : sig
    type ('req, 'resp) request =
      { encode_req : 'req -> Call.t
      ; decode_resp : Csexp.t -> ('resp, Response.Error.t) result
      }

    type 'payload notification = { encode : 'payload -> Call.t }
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
        -> session_version:('state -> int * int)
        -> menu:Menu.t
        -> 'state Handler.t

      val create : unit -> 'state t

      val registered_procedures : 'a t -> (string * int list) list

      (** A *declaration* of a procedure is a claim that this side of the
          session is able to *initiate* that procedure. Correspondingly,
          *implementing* a procedure enables you to *receive* that procedure
          (and probably do something in response).

          Currently, attempting to both implement and declare the same procedure
          in the same builder will raise. While there is nothing fundamentally
          wrong with allowing this, it is simpler for the initial version
          negotiation to treat all method names uniformly, rather than
          specifying whether a given (set of) generation(s) is implemented or
          declared.

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
end

module Server_notifications : sig
  (** Notification sent from server to client *)

  val log : Message.t Decl.Notification.witness

  val abort : Message.t Decl.Notification.witness
end
