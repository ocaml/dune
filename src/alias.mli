type t

val make : string -> dir:Path.t -> t

val default : dir:Path.t -> t
val runtest : dir:Path.t -> t
val install : dir:Path.t -> t

val dep : t -> ('a, 'a) Build.t
val file : t -> Path.t

module Store : sig
  type t
  val create : unit -> t
end

val add_deps : Store.t -> t -> Path.t list -> unit

type tree = Node of Path.t * tree list

val rules
  :  Store.t
  -> prefixes:Path.t list
  -> tree:tree
  -> Build_interpret.Rule.t list
