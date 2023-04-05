(** Generate opam files from dune-project metadata *)

open Import

(** Given an opam filename, returns the filename of the template file *)
val template_file : Path.t -> Path.t

(** Generate the contents of an opam file. [template] is the filename and
    contents of the template file. *)
val generate :
  Dune_project.t -> Package.t -> template:(Path.t * string) option -> string

val add_rules : Super_context.t -> Dune_project.t -> unit Memo.t

val add_opam_file_rules : Super_context.t -> Dune_project.t -> unit Memo.t
