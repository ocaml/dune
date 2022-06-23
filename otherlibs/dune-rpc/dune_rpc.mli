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
    that uses fibers

    The API in this version is versioned. When using this library, we expect
    that the module corresponding to a particular version is used exclusively.

    While we guarantee stability of the API, we reserve the right to:

    - Add optional arguments to functions
    - Add new fields to records
    - New variant constructors that will not cause runtime errors in existing
      user programs.

    This means that you must refrain from re-exporting any values, constructing
    any records, using any module types as functor arguments, or make non
    exhaustive matches an error to guarantee compatibility. *)

[@@@alert
unstable "The API of this library is not stable and may change without notice."]

[@@@alert "-unstable"]

module V1 : sig
  module Id : sig
    (** Id's for requests, responses, sessions.

        Id's are permitted to be arbitrary s-expressions to allow users pick
        descriptive tokens to ease debugging. *)

    type t

    val make : Csexp.t -> t
  end

  module Response : sig
    module Error : sig
      type kind =
        | Invalid_request
        | Code_error

      type t

      val payload : t -> Csexp.t option

      val message : t -> string

      val kind : t -> kind

      exception E of t
    end

    type t = (Csexp.t, Error.t) result
  end

  module Initialize : sig
    type t

    val create : id:Id.t -> t
  end

  module Loc : sig
    type t

    val start : t -> Lexing.position

    val stop : t -> Lexing.position
  end

  module Path : sig
    type t

    val dune_root : t

    val absolute : string -> t

    val relative : t -> string -> t

    val to_string_absolute : t -> string
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
      type t

      val in_build : t -> string

      val in_source : t -> string
    end

    module Id : sig
      type t

      val compare : t -> t -> Ordering.t

      val hash : t -> int

      val create : int -> t
    end

    module Related : sig
      type t

      val loc : t -> Loc.t

      val message : t -> unit Pp.t
    end

    type t

    val related : t -> Related.t list

    val loc : t -> Loc.t option

    val id : t -> Id.t

    val message : t -> unit Pp.t

    val severity : t -> severity option

    val promotion : t -> Promotion.t list

    (* The list of targets is ordered such that the first element is the
       immediate ("innermost") target being built when the error was
       encountered, which was required by the next element, and so on. *)
    val targets : t -> Target.t list

    (* The directory from which the action producing the error was run, relative
       to the workspace root. This is often, but not always, the directory of
       the first target in [targets].

       If this is [None], then the error does not have an associated error (for
       example, if your opam installation is too old). *)
    val directory : t -> string option

    module Event : sig
      type nonrec t =
        | Add of t
        | Remove of t
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

  module Sub : sig
    type 'a t

    val progress : Progress.t t

    val diagnostic : Diagnostic.Event.t list t
  end

  module Message : sig
    type t

    val payload : t -> Csexp.t option

    val message : t -> string
  end

  (** A [Version_error] is returned on the client-side when a request or
      notification is determined to be invalid due to version negotiation (no
      known method or no common version). *)
  module Version_error : sig
    type t

    val payload : t -> Csexp.t option

    val message : t -> string

    exception E of t
  end

  module Notification : sig
    type 'a t

    (** Request dune to shutdown. The current build job will be cancelled. *)
    val shutdown : unit t
  end

  module Request : sig
    type ('a, 'b) t

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t

    (** format a [dune], [dune-project], or a [dune-workspace] file. The full
        path to the file is necessary so that dune knows the formatting options
        for the project this file is in *)
    val format_dune_file : (Path.t * [ `Contents of string ], string) t

    (** Promote a file. *)
    val promote : (Path.t, unit) t

    (** Returns the location of the build directory for the current build. *)
    val build_dir : (unit, Path.t) t
  end

  module Client : sig
    module type S = sig
      (** Rpc client *)

      type t

      type 'a fiber

      type chan

      module Handler : sig
        type t

        val create :
             ?log:(Message.t -> unit fiber)
          -> ?abort:(Message.t -> unit fiber)
               (** If [abort] is called, the server has terminated the
                   connection due to a protocol error. This should never be
                   called unless there's a server side bug. *)
          -> unit
          -> t
      end

      (** Individual RPC procedures are versioned beyond the larger API version.
          At session startup, the server and client exchange version information
          for each method ("negotiation"), setting on a common version for each
          (if possible) to produce a "version menu".

          To initiate a method, then, that method must be looked up in the
          version menu to determine the correct protocol for this session. This
          module stages this pattern to share the lookup for all calls to the
          same procedure.

          For lower-level design details, see [doc/dev/rpc-versioning.md] in the
          main dune repository. *)
      module Versioned : sig
        type 'a notification

        type ('a, 'b) request

        (** [prepare_request client r] checks the request [r] against the
            negotiated version menu, giving a versioned request as a result.

            This function does not initiate any communication with the server.
            However, as this function must check the version menu, it cannot
            complete until after version negotiation, and so returns a [fiber]. *)
        val prepare_request :
             t
          -> ('a, 'b) Request.t
          -> (('a, 'b) request, Version_error.t) result fiber

        (** See [prepare_request]. *)
        val prepare_notification :
             t
          -> 'a Notification.t
          -> ('a notification, Version_error.t) result fiber
      end

      (** [request ?id client decl req] send a request [req] specified by [decl]
          to [client]. If [id] is [None], it will be automatically generated. *)
      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Versioned.request
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Versioned.notification -> 'a -> unit fiber

      (** [disconnected client] produces a fiber that only becomes determined
          when the session is ended from the server side (such as if the build
          server is killed entirely). *)
      val disconnected : t -> unit fiber

      module Stream : sig
        (** Control for a polling loop *)

        type 'a t

        (** [cancel t] notify the server that we are stopping our polling loop.
            It is an error to call [next] after [cancel] *)
        val cancel : _ t -> unit fiber

        (** [next t] poll for the next value. It is an error to call [next]
            again until the previous [next] terminated. If [next] returns
            [None], subsequent calls to [next] is forbidden. *)
        val next : 'a t -> 'a option fiber
      end

      (** [poll client sub] Initialize a polling loop for [sub] *)
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

      (** [connect ?on_handler session init ~f] connect to [session], initialize
          with [init] and call [f] once the client is initialized. [handler] is
          called for some notifications sent to [session] *)
      val connect :
           ?handler:Handler.t
        -> chan
        -> Initialize.t
        -> f:(t -> 'a fiber)
        -> 'a fiber
    end

    (** Functor to create a client implementation *)
    module Make (Fiber : sig
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
    end) (Chan : sig
      type t

      (* [write t x] writes the s-expression when [x] is [Some _], and closes
         the session if [x = None] *)
      val write : t -> Csexp.t list option -> unit Fiber.t

      (* [read t] attempts to read from [t]. If an s-expression is read, it is
         returned as [Some sexp], otherwise [None] is returned and the session
         is closed. *)
      val read : t -> Csexp.t option Fiber.t
    end) : S with type 'a fiber := 'a Fiber.t and type chan := Chan.t
  end

  module Where : sig
    (** represents the address where a dune rpc instance might be listening *)

    type t =
      [ `Unix of string
      | `Ip of [ `Host of string ] * [ `Port of int ]
      ]

    type error = Invalid_where of string

    exception E of error

    module type S = sig
      type 'a fiber

      val get :
           env:(string -> string option)
        -> build_dir:string
        -> (t option, exn) result fiber

      val default : ?win32:bool -> build_dir:string -> unit -> t
    end

    (** obtain the address from the build directory and environment *)
    module Make (Fiber : sig
      type 'a t

      val return : 'a -> 'a t

      module O : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      end
    end) (_ : sig
      val read_file : string -> (string, exn) result Fiber.t

      val analyze_path :
        string -> ([ `Unix_socket | `Normal_file | `Other ], exn) result Fiber.t
    end) : S with type 'a fiber := 'a Fiber.t
  end

  module Registry : sig
    (** The registry is where all running instances of dune rpc are stored.

        It's used by clients to determine which dune rpc instance corresponds to
        the workspace they're trying to edit. *)

    module Dune : sig
      (** a registered instance of dune. supposedly running and listening to rpc
          connections *)

      type t

      val to_dyn : t -> Dyn.t

      val compare : t -> t -> Ordering.t

      val pid : t -> int

      val where : t -> Where.t

      val root : t -> string
    end

    module Config : sig
      (** The registy directory is located using xdg *)

      type t

      val create : Xdg.t -> t

      val watch_dir : t -> string
    end

    type t

    val create : Config.t -> t

    (** currently detected running instances *)
    val current : t -> Dune.t list

    module Refresh : sig
      (** the result of polling the registry *)

      type t

      val added : t -> Dune.t list

      val removed : t -> Dune.t list

      val errored : t -> (string * exn) list
    end

    (** we can poll the registry efficiently using the following functor *)
    module Poll (Fiber : sig
      type 'a t

      val return : 'a -> 'a t

      val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

      module O : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      end
    end) (_ : sig
      val scandir : string -> (string list, exn) result Fiber.t

      val stat : string -> ([ `Mtime of float ], exn) result Fiber.t

      val read_file : string -> (string, exn) result Fiber.t
    end) : sig
      val poll : t -> (Refresh.t, exn) result Fiber.t
    end
  end
end
