type t

val id : t

val rewrite_build_path_prefix_map : t

val make : (string -> string) -> t

val apply : t -> string -> string

module O : sig
  val ( >>> ) : t -> t -> t
end
