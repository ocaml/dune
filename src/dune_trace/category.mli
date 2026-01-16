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
  | Action
  | Cache
  | Digest

val to_string : t -> string
val of_string : string -> t option
val to_dyn : t -> Dyn.t

module Set : sig
  type cat := t
  type t

  val mem : t -> cat -> bool
  val empty : t
  val of_list : cat list -> t
end
