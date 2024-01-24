open Import

type subdir_stanzas

val or_default : subdir_stanzas -> Predicate_lang.Glob.t Source_dir_status.Map.t
val default : Predicate_lang.Glob.t Source_dir_status.Map.t

type status_map

val eval
  :  Predicate_lang.Glob.t Source_dir_status.Map.t
  -> dirs:Filename.t list
  -> status_map

val status : status_map -> dir:Filename.t -> Source_dir_status.Or_ignored.t

module Dir_map : sig
  type t

  module Per_dir : sig
    type t =
      { sexps : Dune_lang.Ast.t list
      ; subdir_status : subdir_stanzas
      }

    val to_dyn : t -> Dyn.t
    val equal : t -> t -> bool
  end

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val empty : t
  val descend : t -> Filename.t -> t option
  val sub_dirs : t -> Filename.t list
  val merge : t -> t -> t
  val root : t -> Per_dir.t
end

type decoder = { decode : 'a. Dune_lang.Ast.t list -> 'a Dune_lang.Decoder.t -> 'a }

val decode : file:Path.Source.t -> decoder -> Dune_lang.Ast.t list -> Dir_map.t Memo.t
