open Path0
include Path_intf.S
module Table : Hashtbl.S with type key = t

val root : t
val relative : t -> string -> t
val initial_cwd : t
val cwd : unit -> t
val as_local : t -> string
val append_local : t -> Local.t -> t
val of_filename_relative_to_initial_cwd : string -> t
val to_dyn : t -> Dyn.t
