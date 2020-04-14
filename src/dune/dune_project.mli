(** dune-project files *)
open! Stdune

open Import

module Name : sig
  (** Invariants: - Named s -> s <> "" and s does not contain '.' or '/' -
      Anonymous p -> p is a local path in the source tree *)
  type t = private
    | Named of string
    | Anonymous of Path.Source.t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string
  (** Convert to a string that is suitable for human readable messages *)

  val to_encoded_string : t -> string
  (** Convert to/from an encoded string that is suitable to use in filenames *)

  val of_encoded_string : string -> t

  module Infix : Comparator.OPS with type t = t

  module Map : Map.S with type key = t
end

module Project_file : sig
  type t

  val to_dyn : t -> Dyn.t
end

type t

module File_key : sig
  (** File_key encodes the project in a unique way to be used as part of file
      path. *)
  type t

  val to_string : t -> string

  val of_string : string -> t

  module Map : Map.S with type key = t
end

val to_dyn : t -> Dyn.t

val file_key : t -> File_key.t

val packages : t -> Package.t Package.Name.Map.t

val version : t -> string option

val name : t -> Name.t

val root : t -> Path.Source.t

val stanza_parser : t -> Stanza.t list Dune_lang.Decoder.t

val allow_approx_merlin : t -> bool

val generate_opam_files : t -> bool

val dialects : t -> Dialect.DB.t

val explicit_js_mode : t -> bool

val format_config : t -> Format_config.t option

val equal : t -> t -> bool

val hash : t -> int

val file : t -> Path.Source.t
(** Return the path of the project file. *)

module Lang : sig
  val register : Dune_lang.Syntax.t -> Stanza.Parser.t list -> unit
  (** [register id stanzas_parser] register a new language. Users will select
      this language by writing:

      {[ (lang <name> <version>) ]}

      as the first line of their [dune-project] file. [stanza_parsers] defines
      what stanzas the user can write in [dune] files. *)
end

module Extension : sig
  type 'a t

  val register :
       ?experimental:bool
    -> Dune_lang.Syntax.t
    -> ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
    -> ('a -> Dyn.t)
    -> 'a t
  (** [register id parser] registers a new extension. Users will enable this
      extension by writing:

      {[ (using <name> <version> <args>) ]}

      in their [dune-project] file. [parser] is used to describe what [<args>]
      might be. *)

  val register_simple :
       ?experimental:bool
    -> Dune_lang.Syntax.t
    -> Stanza.Parser.t list Dune_lang.Decoder.t
    -> unit
  (** A simple version where the arguments are not used through
      [find_extension_args]. *)
end

val load :
     dir:Path.Source.t
  -> files:String.Set.t
  -> infer_from_opam_files:bool
  -> t option
(** Load a project description from the following directory. [files] is the set
    of files in this directory.

    If [infer_from_opam_files] is true and the directory contains no
    [dune-project] file but contains at least one [>package>.opam] files, then a
    project description is inferred from the opam files. *)

val anonymous : dir:Path.Source.t -> t
(** Create an anonymous project with no package rooted at the given directory *)

val filename : string
(** "dune-project" *)

type created_or_already_exist =
  | Created
  | Already_exist

val lang_stanza : unit -> string
(** Generate an appropriate project [lang] stanza *)

val ensure_project_file_exists : t -> created_or_already_exist
(** Check that the dune-project file exists and create it otherwise. *)

val append_to_project_file : t -> string -> created_or_already_exist
(** Append the following text to the project file *)

val default_dune_language_version : Dune_lang.Syntax.Version.t ref
(** Default language version to use for projects that don't have a
    [dune-project] file. The default value is the latest version of the dune
    language. *)

val set :
  t -> ('a, 'k) Dune_lang.Decoder.parser -> ('a, 'k) Dune_lang.Decoder.parser
(** Set the project we are currently parsing dune files for *)

val get_exn : unit -> (t, 'k) Dune_lang.Decoder.parser

val find_extension_args : t -> 'a Extension.t -> 'a option
(** Find arguments passed to (using). [None] means that the extension was not
    written in dune-project. *)

val set_parsing_context : t -> 'a Dune_lang.Decoder.t -> 'a Dune_lang.Decoder.t

val implicit_transitive_deps : t -> bool

val dune_version : t -> Dune_lang.Syntax.Version.t

val wrapped_executables : t -> bool

val strict_package_deps : t -> bool
