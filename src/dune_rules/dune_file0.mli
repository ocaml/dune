(** First stage of evaluating a dune file. Handles [include], [subdir]
    and the various directory status stanzas. *)

open Import

val fname : Filename.t

type kind =
  | Plain
  | Ocaml_script

type t

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val get_static_sexp : t -> Dune_lang.Ast.t list
val kind : t -> kind
val path : t -> Path.Source.t option
val sub_dirs : t option -> Predicate_lang.Glob.t Source_dir_status.Map.t
val plain : t -> Sub_dirs.Dir_map.t
val sub_dirnames : t -> Filename.t list

val load
  :  dir:Path.Source.t
  -> Source_dir_status.t
  -> Dune_project.t
  -> files:Filename.Set.t
  -> parent:t option
  -> t option Memo.t
