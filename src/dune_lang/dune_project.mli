open Import
open Dune_config
module Pin_stanza := Pin_stanza
module Execution_parameters := Dune_engine.Execution_parameters

module Implicit_transitive_deps : sig
  type t =
    | Enabled
    | Disabled
    | Disabled_with_hidden_includes

  val to_bool : t -> bool
end

type t

val to_dyn : t -> Dyn.t
val packages : t -> Package.t Package.Name.Map.t
val name : t -> Dune_project_name.t
val version : t -> Package_version.t option
val root : t -> Path.Source.t
val stanza_parser : t -> dir:Path.Source.t -> Stanza.t list Decoder.t
val generate_opam_files : t -> bool
val set_generate_opam_files : bool -> t -> t

(** The option [use_standard_c_and_cxx_flags] enables the automatic addition of
    flags necessary to build c++ files with the active C compiler. It also
    disables the automatic addition of C flags from [ocamlc -config] to the
    compiler command line when building C stubs. *)
val use_standard_c_and_cxx_flags : t -> bool option

val dialects : t -> Dialect.DB.t
val explicit_js_mode : t -> bool
val format_config : t -> Format_config.t
val subst_config : t -> Loc.t * Config.Toggle.t
val equal : t -> t -> bool
val hash : t -> int

(** Return the path of the project file. *)
val file : t -> Path.Source.t option

module Lang : sig
  (** [register id stanzas_parser] register a new language. Users will select
      this language by writing:

      {[
        (lang <name> <version>)
      ]}

      as the first line of their [dune-project] file. [stanza_parsers] defines
      what stanzas the user can write in [dune] files. *)

  val register : Syntax.t -> Stanza.Parser.t list -> unit
end

module Extension : sig
  type 'a t

  (** [register id parser] registers a new extension. Users will enable this
      extension by writing:

      {[
        (using <name> <version> <args>)
      ]}

      in their [dune-project] file. [parser] is used to describe what [<args>]
      might be. *)
  val register
    :  Syntax.t
    -> ('a * Stanza.Parser.t list) Decoder.t
    -> ('a -> Dyn.t)
    -> 'a t

  (** A simple version where the arguments are not used through
      [find_extension_args]. *)
  val register_simple : Syntax.t -> Stanza.Parser.t list Decoder.t -> unit

  (** Register experimental extensions that were deleted *)
  val register_deleted : name:string -> deleted_in:Syntax.Version.t -> unit
end

(** Create an anonymous project at the given directory

    Optional arguments:

    - [info] defaults to the empty package info
    - [package] defaults to the empty map of packages *)
val anonymous : dir:Path.Source.t -> Package_info.t -> Package.t Package.Name.Map.t -> t

(** "dune-project" *)
val filename : Filename.t

(** Default language version to use for projects that don't have a
    [dune-project] file. The default value is the latest version of the dune
    language. *)
val default_dune_language_version : Syntax.Version.t ref

val get_exn : unit -> (t, 'k) Decoder.parser
val get : unit -> (t option, 'k) Decoder.parser
val find_extension_version : t -> Syntax.t -> Syntax.Version.t option
val is_extension_set : t -> 'a Extension.t -> bool
val set_parsing_context : t -> 'a Decoder.t -> 'a Decoder.t
val implicit_transitive_deps : t -> Ocaml.Version.t -> Implicit_transitive_deps.t
val dune_version : t -> Syntax.Version.t
val wrapped_executables : t -> bool
val executables_implicit_empty_intf : t -> bool
val accept_alternative_dune_file_name : t -> bool
val strict_package_deps : t -> bool
val cram : t -> bool
val info : t -> Package_info.t
val warnings : t -> Warning.Settings.t
val pins : t -> Pin_stanza.Project.t

(** Update the execution parameters according to what is written in the
    [dune-project] file. *)
val update_execution_parameters : t -> Execution_parameters.t -> Execution_parameters.t

val encode : t -> Dune_sexp.t list
val dune_site_extension : unit Extension.t
val opam_file_location : t -> [ `Relative_to_project | `Inside_opam_directory ]
val allow_approximate_merlin : t -> Loc.t option
val filter_packages : t -> f:(Package.Name.t -> bool) -> t
val including_hidden_packages : t -> Package.t Package.Name.Map.t

module Melange_syntax : sig
  val name : string
end

(** Load a project description from the following directory. [files] is the set
    of files in this directory.

    If [infer_from_opam_files] is true and the directory contains no
    [dune-project] file but contains at least one [<package>.opam] files, then a
    project description is inferred from the opam files. *)

val load
  :  dir:Path.Source.t
  -> files:Filename.Set.t
  -> infer_from_opam_files:bool
  -> load_opam_file_with_contents:
       (contents:string -> Path.Source.t -> Package_name.t -> Package.t)
  -> t option Memo.t

val gen_load
  :  read:(Path.Source.t -> string Memo.t)
  -> dir:Path.Source.t
  -> files:Filename.Set.t
  -> infer_from_opam_files:bool
  -> load_opam_file_with_contents:
       (contents:string -> Path.Source.t -> Package_name.t -> Package.t)
  -> t option Memo.t
