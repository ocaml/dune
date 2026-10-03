(** Generate opam files from dune-project metadata *)

open Import

(** Given an opam filename, returns the filename of the template file *)
val template_file : Path.t -> Path.t

(** Path to the generated opam file in the build directory. *)
val build_path : build_dir:Path.Build.t -> Package.t -> Path.Build.t

(** Whether the authored source view contains an opam file, without consulting
    the rules that generate its build-directory counterpart. *)
val source_file_exists : Package.t -> bool Memo.t

(** Generate the contents of an opam file. [template] is the filename and
    contents of the template file. *)
val generate : Dune_project.t -> Package.t -> template:(Path.t * string) option -> string

val gen_rules
  :  Super_context.t Memo.t
  -> dir:Path.Build.t
  -> loaded_project:Loaded_project.t
  -> src_dir:Source_path.t
  -> Build_config.Gen_rules.t
