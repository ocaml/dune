type t

val make : string -> dir:Path.t -> t

val default : dir:Path.t -> t
val runtest : dir:Path.t -> t

val dep : t -> ('a, 'a) Build_system.Build.t
val file : t -> Path.t

val add_deps : t -> Path.t list -> unit

type tree = Node of Path.t * tree list

val setup_rules : tree -> unit
