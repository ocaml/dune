(** Generate opam files from dune-project metadata *)

open Import

(** Given an opam filename, returns the filename of the template file *)
val template_file : Path.t -> Path.t

(** Generate the contents of an opam file. [template] is the filename and
    contents of the template file. *)
val generate : Dune_project.t -> Package.t -> template:(Path.t * string) option -> string

val gen_rules
  :  Super_context.t Memo.t
  -> dir:Path.Build.t
  -> nearest_src_dir:Source_tree.Dir.t option
  -> src_dir:Path.Source.t
  -> Build_config.Gen_rules.Rules.t option
