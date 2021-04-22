open Stdune

module Conv : module type of Conv

module Id : sig
  type t

  val sexp : t Conv.value

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val make : Sexp.t -> t

  val to_sexp : t -> Sexp.t

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
  type t = Stdune.Loc.t =
    { start : Lexing.position
    ; stop : Lexing.position
    }
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

module Diagnostic : sig
  type severity =
    | Error
    | Warning

  module Promotion : sig
    type t =
      { in_build : string
      ; in_source : string
      }
  end

  type t =
    { targets : Target.t list
    ; message : unit Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    }

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
  end
end

module Build : sig
  module Event : sig
    type t =
      | Start
      | Finish
      | Fail
      | Interrupt
  end
end

module Progress : sig
  type t =
    { complete : int
    ; remaining : int
    }
end

module Message : sig
  type t =
    { payload : Sexp.t option
    ; message : string
    }
end

module Subscribe : sig
  type t =
    | Diagnostics
    | Build_progress
end

module type S = sig
  type t

  type 'a fiber

  type chan

  val request :
       ?id:Id.t
    -> t
    -> ('a, 'b) Decl.request
    -> 'a
    -> ('b, Response.Error.t) result fiber

  val notification : t -> 'a Decl.notification -> 'a -> unit fiber

  module Handler : sig
    type t

    val create :
         ?log:(Message.t -> unit fiber)
      -> ?diagnostic:(Diagnostic.Event.t list -> unit fiber)
      -> ?build_event:(Build.Event.t -> unit fiber)
      -> ?build_progress:(Progress.t -> unit fiber)
      -> ?abort:(Message.t -> unit fiber)
      -> unit
      -> t
  end

  val connect_raw :
       chan
    -> Initialize.Request.t
    -> on_notification:(Call.t -> unit fiber)
    -> f:(t -> 'a fiber)
    -> 'a fiber

  val connect :
       ?handler:Handler.t
    -> chan
    -> Initialize.Request.t
    -> f:(t -> 'a fiber)
    -> 'a fiber
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
end) : S with type 'a fiber := 'a S.Fiber.t and type chan := S.Chan.t

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

module Public : sig
  (** Public requests and notifications *)

  module Request : sig
    type ('a, 'b) t = ('a, 'b) Decl.request

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t
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

  val diagnostic : Diagnostic.Event.t list Decl.notification

  val progress : Progress.t Decl.notification

  val log : Message.t Decl.notification

  val abort : Message.t Decl.notification
end
