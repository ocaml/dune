open Stdune

module Status : sig
  type t =
    | Data_only
    | Normal
    | Vendored
    | Generated

  val to_dyn : t -> Dyn.t

  module Or_ignored : sig
    type nonrec t =
      | Ignored
      | Status of t
  end

  module Map : sig
    type status

    type 'a t =
      { data_only : 'a
      ; vendored : 'a
      ; normal : 'a
      ; generated : 'a
      }

    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val find : 'a t -> status -> 'a

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  end
  with type status := t

  module Set : sig
    type t = bool Map.t

    val all : t

    val normal_only : t
  end
end

type subdir_specifiers

val is_generated : subdir_specifiers -> sub_dir:string -> bool

type compiled_subdir_specifiers

val or_default : subdir_specifiers -> compiled_subdir_specifiers

val default : compiled_subdir_specifiers

type status_map

val eval : compiled_subdir_specifiers -> dirs:string list -> status_map

val status : status_map -> dir:string -> Status.Or_ignored.t

module Dir_map : sig
  type t

  type per_dir =
    { sexps : Dune_lang.Ast.t list
    ; subdir_status : subdir_specifiers
    }

  val descend : t -> string -> t option

  val sub_dirs : t -> string list

  val merge : t -> t -> t

  val root : t -> per_dir
end

val decode : file:Path.Source.t -> Dir_map.t Dune_lang.Decoder.t
