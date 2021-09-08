module Conv : module type of Conv

module Id : sig
  type t

  val sexp : t Conv.value

  val to_dyn : t -> Stdune.Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val make : Csexp.t -> t

  val to_sexp : t -> Csexp.t

  module Set : Stdune.Set.S with type elt = t
end

module Call : sig
  type t =
    { method_ : string
    ; params : Csexp.t
    }

  val create : ?params:Csexp.t -> method_:string -> unit -> t
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
      { payload : Csexp.t option
      ; message : string
      ; kind : kind
      }

    val payload : t -> Csexp.t option

    val message : t -> string

    val kind : t -> kind

    exception E of t

    val to_dyn : t -> Stdune.Dyn.t

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

module Where : sig
  type t =
    [ `Unix of string
    | `Ip of [ `Host of string ] * [ `Port of int ]
    ]

  val of_string : string -> t

  val to_string : t -> string

  val add_to_env : t -> Stdune.Env.t -> Stdune.Env.t

  module type S = sig
    type 'a fiber

    val get : build_dir:string -> t option fiber

    val default : build_dir:string -> t
  end

  module Make (Fiber : sig
    type 'a t

    val return : 'a -> 'a t

    module O : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end
  end) (Sys : sig
    val getenv : string -> string option

    val is_win32 : unit -> bool

    val read_file : string -> string Fiber.t

    val readlink : string -> string option Fiber.t

    val analyze_path :
      string -> [ `Unix_socket | `Normal_file | `Other ] Fiber.t
  end) : S with type 'a fiber := 'a Fiber.t
end

module Loc : sig
  type t = Stdune.Loc.t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

  val start : t -> Lexing.position

  val stop : t -> Lexing.position
end

module Target : sig
  type t =
    | Path of string
    | Alias of string
    | Library of string
    | Executables of string list
    | Preprocess of string list
    | Loc of Loc.t
end

module Path : sig
  type t = string

  val dune_root : t

  val absolute : string -> t

  val relative : t -> string -> t
end

module Diagnostic : sig
  type severity =
    | Error
    | Warning

  module Promotion : sig
    type t =
      { in_build : string
      ; in_source : string
      }

    val in_build : t -> string

    val in_source : t -> string
  end

  module Id : sig
    type t

    val compare : t -> t -> int

    val hash : t -> int

    val create : int -> t
  end

  module Related : sig
    type t =
      { message : unit Pp.t
      ; loc : Loc.t
      }

    val message : t -> unit Pp.t

    val loc : t -> Loc.t
  end

  type t =
    { targets : Target.t list
    ; id : Id.t
    ; message : unit Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    ; related : Related.t list
    }

  val related : t -> Related.t list

  val id : t -> Id.t

  val loc : t -> Loc.t option

  val message : t -> unit Pp.t

  val severity : t -> severity option

  val promotion : t -> Promotion.t list

  val targets : t -> Target.t list

  val directory : t -> string option

  module Event : sig
    type nonrec t =
      | Add of t
      | Remove of t

    val to_dyn : t -> Stdune.Dyn.t
  end
end

module Progress : sig
  type t =
    | Waiting
    | In_progress of
        { complete : int
        ; remaining : int
        }
    | Failed
    | Interrupted
    | Success
end

module Message : sig
  type t =
    { payload : Csexp.t option
    ; message : string
    }

  val payload : t -> Csexp.t option

  val message : t -> string

  val to_sexp_unversioned : t -> Csexp.t
end

module Subscribe : sig
  type t =
    | Diagnostics
    | Build_progress
end

module Build_outcome : sig
  type t =
    | Success
    | Failure
end

module Status : sig
  module Menu : sig
    type t =
      | Uninitialized
      | Menu of (string * int) list
  end

  type t = { clients : (Id.t * Menu.t) list }
end

module Decl : sig
  module Request : sig
    type ('a, 'b) t

    type ('a, 'b) witness

    val witness : ('a, 'b) t -> ('a, 'b) witness
  end

  module Notification : sig
    type 'a t

    type 'a witness

    val witness : 'a t -> 'a witness
  end

  type ('a, 'b) request = ('a, 'b) Request.t

  type 'a notification = 'a Notification.t

  module For_tests : sig
    module Request : sig
      module type S = sig
        type req

        type resp

        type wire_req

        type wire_resp

        val version : int

        val req : wire_req Conv.value

        val resp : wire_resp Conv.value

        val upgrade_req : wire_req -> req

        val downgrade_req : req -> wire_req

        val upgrade_resp : wire_resp -> resp

        val downgrade_resp : resp -> wire_resp
      end

      type ('req, 'resp) gen

      val make_gen :
           (module S with type req = 'req and type resp = 'resp)
        -> ('req, 'resp) gen

      val make :
           method_:string
        -> generations:('req, 'resp) gen list
        -> ('req, 'resp) Request.t
    end

    module Notification : sig
      module type S = sig
        type model

        type wire

        val version : int

        val sexp : wire Conv.value

        val upgrade : wire -> model

        val downgrade : model -> wire
      end

      type 'payload gen

      val make_gen : (module S with type model = 'payload) -> 'payload gen

      type 'payload t

      val make :
           method_:string
        -> generations:'payload gen list
        -> 'payload Notification.t
    end
  end
end

module type Fiber = sig
  type 'a t

  val return : 'a -> 'a t

  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

  val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

  val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

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

module Client : sig
  module type S = sig
    type t

    type 'a fiber

    type chan

    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Decl.Request.witness
      -> 'a
      -> ('b, Response.Error.t) result fiber

    val notification :
         t
      -> 'a Decl.Notification.witness
      -> 'a
      -> (unit, Response.Error.t) result fiber

    val disconnected : t -> unit fiber

    module Batch : sig
      type t

      type client

      val create : client -> t

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Decl.Request.witness
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification :
           t
        -> 'a Decl.Notification.witness
        -> 'a
        -> (unit, Response.Error.t) result fiber

      val submit : t -> unit fiber
    end
    with type client := t

    module Handler : sig
      type t

      val create :
           ?log:(Message.t -> unit fiber)
        -> ?diagnostic:(Diagnostic.Event.t list -> unit fiber)
        -> ?build_progress:(Progress.t -> unit fiber)
        -> ?abort:(Message.t -> unit fiber)
        -> unit
        -> t
    end

    val connect :
         ?handler:Handler.t
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber

    module For_tests : sig
      type proc =
        | Request : ('a, 'b) Decl.request -> proc
        | Notification : 'a Decl.notification -> proc

      val connect :
           ?handler:Handler.t
        -> extra_procedures:proc list
        -> chan
        -> Initialize.Request.t
        -> f:(t -> 'a fiber)
        -> 'a fiber
    end
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

module Versioned : sig
  module Menu : sig
    type t

    val default : t

    (** For each method known by both local and remote, choose the highest
        common version number. Returns [None] if the resulting menu would be
        empty. *)
    val select_common :
         local_versions:Stdune.Int.Set.t Stdune.String.Map.t
      -> remote_versions:(string * int list) list
      -> t option

    val of_list : (string * int) list -> (t, string * int * int) result

    val to_list : t -> (string * int) list

    val to_dyn : t -> Stdune.Dyn.t
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

module Procedures : sig
  (** Procedures with generations for server impl *)
  module Public : sig
    val ping : (unit, unit) Decl.Request.t

    val diagnostics : (unit, Diagnostic.t list) Decl.Request.t

    val shutdown : unit Decl.Notification.t

    val subscribe : Subscribe.t Decl.Notification.t

    val unsubscribe : Subscribe.t Decl.Notification.t

    val format_dune_file :
      (Path.t * [ `Contents of string ], string) Decl.Request.t

    val promote : (Path.t, unit) Decl.Request.t
  end

  module Internal : sig
    val build : (string list, Build_outcome.t) Decl.Request.t

    val status : (unit, Status.t) Decl.Request.t
  end

  module Server_side : sig
    val abort : Message.t Decl.Notification.t

    val log : Message.t Decl.Notification.t

    val progress : Progress.t Decl.Notification.t

    val diagnostic : Diagnostic.Event.t list Decl.Notification.t
  end
end

module Public : sig
  (** Public requests and notifications *)

  module Request : sig
    type ('a, 'b) t = ('a, 'b) Decl.Request.witness

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t

    val format_dune_file : (Path.t * [ `Contents of string ], string) t

    val promote : (Path.t, unit) t
  end

  module Notification : sig
    type 'a t = 'a Decl.Notification.witness

    val subscribe : Subscribe.t t

    val unsubscribe : Subscribe.t t

    val shutdown : unit t
  end
end

module Server_notifications : sig
  (** Notification sent from server to client *)

  val diagnostic : Diagnostic.Event.t list Decl.Notification.witness

  val progress : Progress.t Decl.Notification.witness

  val log : Message.t Decl.Notification.witness

  val abort : Message.t Decl.Notification.witness
end
