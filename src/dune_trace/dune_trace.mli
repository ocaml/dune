open Stdune

module Category : sig
  type t =
    | Rpc
    | Gc
    | Fd
    | Sandbox
    | Persistent
    | Process
    | Rules
    | Pkg
    | Scheduler
    | Promote
    | Build
    | Debug
    | Config
    | File_watcher
    | Diagnostics
    | Log
    | Cram

  val of_string : string -> t option
end

module Event : sig
  module Async : sig
    type t
    type data

    val create_sandbox : loc:Loc.t -> data
    val fetch : url:string -> target:Path.t -> checksum:string option -> data
  end

  type t

  val evalauted_rules : rule_total:int -> t

  module Exit_status : sig
    type error =
      | Failed of int
      | Signaled of Signal.t

    type t = (int, error) result
  end

  type targets =
    { root : Path.Build.t
    ; files : Filename.Set.t
    ; dirs : Filename.Set.t
    }

  val process
    :  name:string option
    -> started_at:Time.t
    -> targets:targets option
    -> categories:string list
    -> pid:Pid.t
    -> exit:Exit_status.t
    -> prog:string
    -> process_args:string list
    -> dir:Path.t option
    -> stdout:string
    -> stderr:string
    -> times:Proc.Times.t
    -> t

  val persistent
    :  file:Path.t
    -> module_:string
    -> [ `Save | `Load ]
    -> start:Time.t
    -> stop:Time.t
    -> t

  val scan_source : name:string -> start:Time.t -> stop:Time.t -> dir:Path.Source.t -> t
  val scheduler_idle : unit -> t
  val config : version:string option -> t
  val gc : unit -> t
  val fd_count : unit -> t option
  val promote : Path.Build.t -> Path.Source.t -> t

  type alias =
    { dir : Path.Source.t
    ; name : string
    ; recursive : bool
    ; contexts : string list
    }

  val resolve_targets : Path.t list -> alias list -> t
  val load_dir : Path.t -> t
  val log : Log.Message.t -> t

  val file_watcher
    :  [ `File of Path.t * [ `Created | `Deleted | `File_changed | `Unknown ]
       | `Queue_overflow
       | `Sync of int
       | `Watcher_terminated
       ]
    -> t

  val error
    :  Loc.t option
    -> [< `Fatal | `User ]
    -> Exn.t
    -> Printexc.raw_backtrace option
    -> Dyn.t list
    -> t

  module Rpc : sig
    type stage =
      [ `Start
      | `Stop
      ]

    val session : id:int -> stage -> t

    val message
      :  [ `Request of Sexp.t | `Notification ]
      -> meth_:string
      -> id:int
      -> stage
      -> t

    val packet_read : id:int -> success:bool -> error:string option -> t
    val packet_write : id:int -> count:int -> t
    val accept : success:bool -> error:string option -> t
    val close : id:int -> t
  end

  module Cram : sig
    type times =
      { real : Time.Span.t
      ; system : Time.Span.t
      ; user : Time.Span.t
      }

    type command =
      { command : string list
      ; times : times
      }

    val test : command list -> t
  end
end

module Out : sig
  type t

  val create : Category.t list -> Path.t -> t
  val emit : t -> Event.t -> unit
  val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
  val finish : t -> Event.Async.t option -> unit
end

val global : unit -> Out.t option
val set_global : Out.t -> unit
val always_emit : Event.t -> unit
val emit : Category.t -> (unit -> Event.t) -> unit
val emit_all : Category.t -> (unit -> Event.t list) -> unit

module Private : sig
  module Fd_count : sig
    type t =
      | Unknown
      | This of int

    val get : unit -> t
  end

  module Buffer : module type of Buffer
end
