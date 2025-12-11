open Stdune

type t

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

val global : unit -> t option
val set_global : t -> unit
val create : dst -> t
val record_gc_and_fd : t -> unit

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
    -> started_at:float
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
    -> start:float
    -> stop:float
    -> t

  val scan_source : name:string -> start:float -> stop:float -> dir:Path.Source.t -> t
  val scheduler_idle : unit -> t
  val config : version:string option -> t

  module Rpc : sig
    type stage =
      | Start
      | Stop

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
end

val emit : t -> Event.t -> unit
val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
val finish : Event.Async.t option -> unit
val flush : t -> unit

module Private : sig
  module Fd_count : sig
    type t =
      | Unknown
      | This of int

    val get : unit -> t
  end
end
