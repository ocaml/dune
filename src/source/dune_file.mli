(** First stage of evaluating a dune file. Handles [include], [subdir]
    and the various directory status stanzas. *)

open Import

val statically_evaluated_stanzas : string list
val fname : Filename.t

type kind =
  | Plain
  | Ocaml_script of string

type t

val to_dyn : t -> Dyn.t

(** The contents of the dune file without the OCaml syntax *)
val get_static_sexp : t -> Dune_lang.Ast.t list

val kind : t -> kind

(** The workspace path to the dune file, if any. *)
val path : t -> Path.Source.t option

val source_path : t -> Source_path.t option
val filename : t -> Filename.t option

(** The required literal child directory of an Opam stanza. *)
val opam_dir : Filename.t Dune_lang.Decoder.fields_parser

val sub_dir_status : t -> Source_dir_status.Spec.t

(** The location of the (dirs ...) stanza if present *)
val dirs_stanza_loc : t -> Loc.t option

module Files : sig
  type t

  val default : t
  val eval : t -> files:Filename.Array.Set.t -> Filename.Array.Set.t
end

val files : t -> Files.t

(** Directories introduced via [(subdir ..)] *)
val sub_dirnames : t -> Filename.Array.Set.t

val load
  :  dir:Source_tree_file.Dir.t
  -> Source_dir_status.t
  -> Dune_project.t
  -> files:Filename.Array.Set.t
  -> parent:t option
  -> t option Memo.t
