open! Import


module Dir : sig
  type t

  val path     : t -> Path.t
  val files    : t -> String_set.t
  val sub_dirs : t -> t String_map.t
end

type t

val load : Path.t -> t

val fold : t -> init:'a -> f:(Dir.t -> 'a -> 'a) -> 'a

val root : t -> Dir.t

val find_dir : t -> Path.t -> Dir.t option

val exists : t -> Path.t -> bool
val file_exists : t -> Path.t -> string -> bool
