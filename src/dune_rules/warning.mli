(** General warning mechanism for dune rules *)

open Import

type t

module Settings : sig
  (** Settings to disable/enable specific warnings in a project *)

  type warning := t
  type t

  val to_dyn : t -> Dyn.t
  val empty : t
  val decode : t Dune_sexp.Decoder.t
  val active : t -> warning -> Syntax.Version.t -> Config.Toggle.t
end

val name : t -> string

(** Warn whenever [(name <name>)]) is missing from the [dune-project] file *)
val missing_project_name : t

val escaping_paths_in_install_stanza : t
val no_keep_locs : t
