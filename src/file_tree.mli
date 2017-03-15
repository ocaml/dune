open! Import


module Dir : sig
  type t

  val path     : t -> Path.t
  val files    : t -> String_set.t
  val sub_dirs : t -> t String_map.t
end

type t

val load : Path.t -> t

type 'a fold_callback_result =
  | Cont            of 'a
  | Dont_recurse_in of String_set.t * 'a

val fold : t -> init:'a -> f:(Dir.t -> 'a -> 'a fold_callback_result) -> 'a

val root : t -> Dir.t

val find_dir : t -> Path.t -> Dir.t option

val exists : t -> Path.t -> bool
val file_exists : t -> Path.t -> string -> bool

val files_recursively_in : t -> ?prefix_with:Path.t -> Path.t -> Path.Set.t
