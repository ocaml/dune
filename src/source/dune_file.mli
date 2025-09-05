(** First stage of evaluating a dune file. Handles [include], [subdir]
    and the various directory status stanzas. *)

open Import

val statically_evaluated_stanzas : string list
val fname : Filename.t

type kind =
  | Plain
  | Ocaml_script

type t

val to_dyn : t -> Dyn.t

(** The contents of the dune file without the OCaml syntax *)
val get_static_sexp : t -> Dune_lang.Ast.t list

val kind : t -> kind

(** The path to the dune file. If [kind = Ocaml_script], then this always
    returns [Some p] where [p] is path to the script *)
val path : t -> Path.Source.t option

val sub_dir_status : t -> Source_dir_status.Spec.t

(** Directories introduced via [(subdir ..)] *)
val sub_dirnames : t -> Filename.t list

val load
  :  dir:Path.Source.t
  -> Source_dir_status.t
  -> Dune_project.t
  -> files:Filename.Set.t
  -> parent:t option
  -> t option Memo.t
