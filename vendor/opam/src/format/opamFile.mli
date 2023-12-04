(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Handles all OPAM file formats as record types and submodules, conversion to
    and from syntax *)

open OpamParserTypes.FullPos
open OpamTypes

(** Functions to read and write OPAM configuration files in a typed way *)

(** Associate a type to a filename through a phantom type *)
type 'a t = private filename

type 'a typed_file = 'a t

val make: filename -> 'a t
val filename: 'a t -> filename
val to_string: 'a t -> string
val exists: 'a t -> bool

(** All Configuration files satisfy this signature *)
module type IO_FILE = sig

  (** File contents *)
  type t

  val format_version: OpamVersion.t

  (** Empty file *)
  val empty: t

  (** Write some contents to a file *)
  val write: t typed_file -> t -> unit

  (** Read file contents. Raise an error if the file does not exist. *)
  val read: t typed_file -> t

  (** Returns [None] on non-existing file *)
  val read_opt: t typed_file -> t option

  (** Read file contents. Return [empty] if the file does not exist. *)
  val safe_read: t typed_file -> t

  val read_from_channel: ?filename:t typed_file -> in_channel -> t

  val read_from_string: ?filename:t typed_file -> string -> t

  val write_to_channel: ?filename:t typed_file -> out_channel -> t -> unit

  val write_to_string: ?filename:t typed_file -> t -> string

end

(* Error less [IO_FILE] read functions. *)
module type BestEffortRead = sig
  type t
  val read: t typed_file -> t
  val read_opt: t typed_file -> t option
  val safe_read: t typed_file -> t
  val read_from_channel: ?filename:t typed_file -> in_channel -> t
  val read_from_string: ?filename:t typed_file -> string -> t
end

(** Lines of space-separated words. *)
module Lines: IO_FILE with type t = string list list

(** Command wrappers for package scripts *)
module Wrappers: sig
  type t = {
    pre_build : command list;
    wrap_build : command list;
    post_build : command list;
    pre_install : command list;
    wrap_install : command list;
    post_install : command list;
    pre_remove : command list;
    wrap_remove : command list;
    post_remove : command list;
    pre_session : command list;
    post_session : command list;
  }

  val pre_build: t -> command list
  val wrap_build: t -> command list
  val post_build: t -> command list
  val pre_install: t -> command list
  val wrap_install: t -> command list
  val post_install: t -> command list
  val pre_remove: t -> command list
  val wrap_remove: t -> command list
  val post_remove: t -> command list
  val pre_session: t -> command list
  val post_session: t -> command list

  val with_pre_build: command list -> t -> t
  val with_wrap_build: command list -> t -> t
  val with_post_build: command list -> t -> t
  val with_pre_install: command list -> t -> t
  val with_wrap_install: command list -> t -> t
  val with_post_install: command list -> t -> t
  val with_pre_remove: command list -> t -> t
  val with_wrap_remove: command list -> t -> t
  val with_post_remove: command list -> t -> t
  val with_pre_session: command list -> t -> t
  val with_post_session: command list -> t -> t

  val empty : t

  val add: outer:t -> inner:t -> t
end

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Current root version *)
  val root_version: opam_version

  (** OCaml switch updates *)
  val with_switch: switch -> t -> t
  val with_switch_opt: switch option -> t -> t

  (** The list of switches, ordered by most recently used first *)
  val with_installed_switches: switch list -> t -> t

  (** Repository updates *)
  val with_repositories: repository_name list -> t -> t

  (** Update opam-version *)
  val with_opam_version: OpamVersion.t -> t -> t
  val with_opam_root_version: OpamVersion.t -> t -> t

  val with_criteria: (solver_criteria * string) list -> t -> t
  val with_best_effort_prefix: string -> t -> t
  val with_best_effort_prefix_opt: string option-> t -> t

  val with_solver: arg list -> t -> t
  val with_solver_opt: arg list option -> t -> t

  val with_jobs: int -> t -> t
  val with_jobs_opt: int option -> t -> t
  val with_dl_tool: arg list -> t -> t
  val with_dl_tool_opt: arg list option -> t -> t
  val with_dl_jobs: int -> t -> t
  val with_dl_cache: url list -> t -> t

  val with_wrappers: Wrappers.t -> t -> t
  val with_global_variables:
    (variable * variable_contents * string) list -> t -> t
  val with_eval_variables:
    (variable * string list * string) list -> t -> t
  val with_validation_hook_opt:
    arg list option -> t -> t
  val with_default_compiler:
    formula -> t -> t
  val with_default_invariant:
    formula -> t -> t
  val with_depext: bool -> t -> t
  val with_depext_run_installs: bool -> t -> t
  val with_depext_cannot_install: bool -> t -> t
  val with_depext_bypass: OpamSysPkg.Set.t -> t -> t
  val with_sys_pkg_manager_cmd: filename OpamStd.String.Map.t -> t -> t

  val with_swh_fallback: bool -> t -> t

  (** Return the opam version *)
  val opam_version: t  -> opam_version

  (** Return the opam root version, if not defined returns the default version
      value [2.1~~previous] *)
  val opam_root_version: t -> opam_version

  (** Return the opam root version if defined *)
  val opam_root_version_opt: t -> opam_version option

  (** Return the list of repository *)
  val repositories: t  -> repository_name list

  (** Return the OCaml switch *)
  val switch: t -> switch option

  val previous_switch : t -> switch option

  val installed_switches: t -> switch list

  (** Return the number of jobs defined *)
  val jobs: t -> int option

  val dl_tool: t -> arg list option

  (** Return the number of download jobs *)
  val dl_jobs: t -> int

  val dl_cache: t -> url list

  val criteria: t -> (solver_criteria * string) list

  val best_effort_prefix: t -> string option

  val solver: t -> arg list option

  val wrappers: t -> Wrappers.t

  (** variable, value, docstring *)
  val global_variables: t -> (variable * variable_contents * string) list

  (** variable, command, docstring *)
  val eval_variables: t -> (variable * string list * string) list

  val validation_hook: t -> arg list option

  val default_compiler: t -> formula
  val default_invariant: t -> formula

  val depext: t -> bool
  val depext_run_installs: t -> bool
  val depext_cannot_install: t -> bool
  val depext_bypass: t -> OpamSysPkg.Set.t

  val sys_pkg_manager_cmd: t -> filename OpamStd.String.Map.t

  (* Enable or disable Software Heritage fallback for unreachable package
     sources *)
  val swh_fallback: t -> bool

  val fields: (string * (t, value) OpamPp.field_parser) list

  (** All file fields as print-AST, Fields within sections are
      accessed through dot-separated paths *)
  val to_list: ?filename:'a typed_file -> t -> (string * value) list

  module BestEffort: BestEffortRead with type t := t

  (** Raw read the config file to extract [opam-root-version] field value. *)
  val raw_root_version: 'a typed_file -> OpamVersion.t option
end

(** Init config file [/etc/opamrc] *)
module InitConfig: sig
  include IO_FILE

  val opam_version: t -> opam_version
  val repositories: t -> (repository_name * (url * trust_anchors option)) list
  val default_compiler: t -> formula
  val default_invariant: t -> formula
  val jobs: t -> int option
  val dl_tool: t -> arg list option
  val dl_jobs: t -> int option
  val dl_cache: t -> url list
  val solver_criteria: t -> (solver_criteria * string) list
  val solver: t -> arg list option
  val wrappers: t -> Wrappers.t
  val global_variables: t -> (variable * variable_contents * string) list
  val eval_variables: t -> (variable * string list * string) list
  val recommended_tools: t -> (string list * string option * filter option) list
  val required_tools: t -> (string list * string option * filter option) list
  val init_scripts: t -> ((string * string) * filter option) list

  val with_opam_version: opam_version -> t -> t
  val with_repositories:
    (repository_name * (url * trust_anchors option)) list -> t -> t
  val with_default_compiler: formula -> t -> t
  val with_default_invariant: formula -> t -> t
  val with_jobs: int option -> t -> t
  val with_dl_tool: arg list option -> t -> t
  val with_dl_jobs: int option -> t -> t
  val with_dl_cache: url list -> t -> t
  val with_solver_criteria: (solver_criteria * string) list -> t -> t
  val with_solver: arg list option -> t -> t
  val with_wrappers: Wrappers.t -> t -> t
  val with_global_variables: (variable * variable_contents * string) list -> t -> t
  val with_eval_variables: (variable * string list * string) list -> t -> t
  val with_recommended_tools: (string list * string option * filter option) list -> t -> t
  val with_required_tools: (string list * string option * filter option) list -> t -> t
  val with_init_scripts: ((string * string) * filter option) list -> t -> t

  (** [add t1 t2] is [t2], with the field values falling back to those of [t1]
      when not set in [t2] *)
  val add: t -> t -> t
end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig

  include IO_FILE

  val create: string -> t

  (** Create an abstract description file from a string *)
  val of_string: t typed_file -> string -> t

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the body *)
  val body: t -> string

  (** Return the full description *)
  val full: t -> string

end

(** {2 Urls for OPAM repositories} *)
module URL: sig

  include IO_FILE

  val create:
    ?mirrors:url list -> ?checksum:OpamHash.t list ->
    ?swhid:OpamSWHID.t -> ?subpath:subpath ->
    url -> t

  (** URL address *)
  val url: t -> url

  val mirrors: t -> url list

  (** Archive checksum *)
  val checksum: t -> OpamHash.t list
  val swhid: t -> OpamSWHID.t option

  (** Constructor *)
  val with_url: url -> t -> t
  val with_checksum: OpamHash.t list -> t -> t
  val with_mirrors: OpamUrl.t list -> t -> t
  val with_swhid: OpamSWHID.t -> t -> t
  val with_swhid_opt: OpamSWHID.t option -> t -> t
  val with_subpath: subpath -> t -> t
  val with_subpath_opt: subpath option -> t -> t

  val subpath: t -> subpath option

end

(** OPAM files *)
module OPAM: sig

  type t = private {
    opam_version: opam_version;

    (* Package ident *)
    name       : name option;
    version    : version option;

    (* Relationships; solver and availability info *)
    depends    : filtered_formula;
    depopts    : filtered_formula;
    conflicts  : filtered_formula;
    conflict_class : name list;
    available  : filter;
    flags      : package_flag list;
    env        : env_update list;

    (* Build instructions *)
    build      : command list;
    run_test   : command list;
    install    : command list;
    remove     : command list;

    (* Auxiliary data affecting the build *)
    substs     : basename list;
    patches    : (basename * filter option) list;
    build_env  : env_update list;
    features   : (OpamVariable.t * filtered_formula * string) list;
    extra_sources: (basename * URL.t) list;

    (* User-facing data used by opam *)
    messages   : (string * filter option) list;
    post_messages: (string * filter option) list;
    depexts    : (OpamSysPkg.Set.t * filter) list;
    libraries  : (string * filter option) list;
    syntax     : (string * filter option) list;
    dev_repo   : url option;
    pin_depends: (package * url) list;

    (* Package database details *)
    maintainer : string list;
    author     : string list;
    license    : string list;
    tags       : string list;
    homepage   : string list;
    doc        : string list;
    bug_reports: string list;

    (* Extension fields (x-foo: "bar") *)
    extensions : value OpamStd.String.Map.t;

    (* Extra sections *)
    url        : URL.t option;
    descr      : Descr.t option;

    (* Related metadata directory (not an actual field of the file)
       This can be used to locate e.g. the files/ overlays.
       If the repository is specified, the string is a relative path from its
       root. It should otherwise be an absolute path. *)
    metadata_dir: (repository_name option * string) option;

    (* Names and hashes of the files below files/ *)
    extra_files: (OpamFilename.Base.t * OpamHash.t) list option;

    (* Origin with the extension *)
    locked: string option;

    format_errors: (string * OpamPp.bad_format) list;

    (* Deprecated, for compat and proper linting *)
    ocaml_version: (OpamFormula.relop * string) OpamFormula.formula option;
    os         : (bool * string) generic_formula;
    deprecated_build_test : command list;
    deprecated_build_doc  : command list;
  }

  include IO_FILE with type t := t

  val empty: t

  (** Create an opam file *)
  val create: package -> t

  (** Returns the opam value (including url, descr) with all non-effective (i.e.
      user-directed information that doesn't change opam's view on the package)
      fields set to their empty values. Useful for comparisons.
      @param ?modulo_state if [true], eliminates the fields relying on the state
      of the switch (depends, available, …). This is [false] by default. *)
  val effective_part: ?modulo_state:bool -> t -> t

  (** Returns true if the effective parts of the two package definitions are
      equal.
      @param ?modulo_state if [true], considers the fields relying on the state
      of the switch (depends, available, …) equal. This is [false] by default *)
  val effectively_equal: ?modulo_state:bool -> t -> t -> bool

  (** Compares two package definitions, ignoring the virtual fields bound to
      file location ([metadata_dir]...) *)
  val equal: t -> t -> bool

  (** Prints the format errors that were found when the file was read *)
  val print_errors: ?file:t typed_file -> t -> unit

  (** Get OPAM version. *)
  val opam_version: t -> opam_version

  (** Package name *)
  val name: t -> name
  val name_opt: t -> name option

  (** Package version *)
  val version: t -> version
  val version_opt: t -> version option

  (** The informations in both the name and version fields, as a package *)
  val package: t -> package

  (** Availability formula (OS + compiler constraints) *)
  val available: t -> filter

  (** Package maintainer(s) *)
  val maintainer: t -> string list

  (** File substitutions *)
  val substs: t -> basename list

  (** List of environment variables to set-up for the build *)
  val build_env: t -> env_update list

  (** List of command to run for building the package *)
  val build: t -> command list

  (** List of command to run for installing the package *)
  val install: t -> command list

  (** List of command to run for removing the package *)
  val remove: t -> command list

  (** Package dependencies *)
  val depends: t -> filtered_formula

  (** Optional dependencies *)
  val depopts: t -> filtered_formula

  (** External dependencies *)
  val depexts: t -> (OpamSysPkg.Set.t * filter) list

  val extra_sources: t -> (basename * URL.t) list

  (** All extended "x-" fields as a map *)
  val extensions: t -> value OpamStd.String.Map.t

  (** Parse a single extended field (reports proper file position) *)
  val extended: t -> string -> (value -> 'a) -> 'a option

  val with_messages: (string * filter option) list -> t -> t

  val with_post_messages: (string * filter option) list -> t -> t

  (** Package conflicts *)
  val conflicts: t -> filtered_formula

  val conflict_class: t -> name list

  (** Contents of the 'features' field *)
  val features: t -> (OpamVariable.t * filtered_formula * string) list

  (** List of exported libraries *)
  val libraries: t -> (string * filter option) list

  (** List of exported syntax extensions *)
  val syntax: t -> (string * filter option) list

  (** Patches *)
  val patches: t -> (basename * filter option) list

  (** Homepage(s) *)
  val homepage: t -> string list

  (** Author(s) *)
  val author: t -> string list

  (** License(s) *)
  val license: t -> string list

  (** API documentation *)
  val doc: t -> string list

  (** Classification tags *)
  val tags: t -> string list

  (** Commands to build and run the tests *)
  val run_test: t -> command list

  (** Commands to build the documentation *)
  val deprecated_build_doc: t -> command list

  (** Commands to build the tests *)
  val deprecated_build_test: t -> command list

  (** Messages to display before taking action *)
  val messages: t -> (string * filter option) list

  (** Messages to display at end of install *)
  val post_messages: t -> (string * filter option) list

  (** Where to post bug reports. *)
  val bug_reports: t -> string list

  (** The package flags that are present for this package. *)
  val flags: t -> package_flag list

  (** Check the package for the given flag. Allows flags specified through tags
      for compatibility *)
  val has_flag: package_flag -> t -> bool

  (** The environment variables that this package exports *)
  val env: t -> env_update list

  val descr: t -> Descr.t option

  val synopsis: t -> string option
  val descr_body: t -> string option

  val url: t -> URL.t option

  val get_url: t -> url option

  (** Related metadata directory (either repository name + relative path, or
      absolute path; not an actual field of the file, linked to the file
      location).
      This can be used to locate e.g. the files/ overlays *)
  val metadata_dir: t -> (repository_name option * string) option

  (** Gets the resolved metadata dir, given a mapping of repository names to
      their roots *)
  val get_metadata_dir:
    repos_roots:(repository_name -> dirname) -> t -> dirname option

  (** Names and hashes of the files below files/ *)
  val extra_files: t -> (OpamFilename.Base.t * OpamHash.t) list option

  (** From locked opam file *)
  val locked: t -> string option

  (** Looks up the extra files, and returns their full paths, relative path to
      the package source, and hash. Doesn't check the hashes. *)
  val get_extra_files:
    repos_roots:(repository_name -> dirname) ->
    t -> (filename * basename * OpamHash.t) list

  (** Returns the errors that were found when parsing the file, associated to
      their fields (that were consequently ignored) *)
  val format_errors: t -> (string * OpamPp.bad_format) list

  (** Sets the opam version *)
  val with_opam_version: opam_version -> t -> t

  (** The package source repository address *)
  val dev_repo: t -> url option

  val pin_depends: t -> (package * url) list

  (** construct as [name] *)
  val with_name: name -> t -> t
  val with_name_opt: name option -> t -> t

  (** construct as [version] *)
  val with_version: version -> t -> t
  val with_version_opt: version option -> t -> t

  (** Construct as [depends] *)
  val with_depends: filtered_formula -> t -> t

  (** Construct as [depopts] *)
  val with_depopts: filtered_formula -> t -> t

  val with_conflicts: filtered_formula -> t -> t

  val with_conflict_class: name list -> t -> t

  val with_features: (OpamVariable.t * filtered_formula * string) list -> t -> t

  (** Construct as [build] *)
  val with_build: command list -> t -> t

  val with_run_test: command list -> t -> t

  val with_install: command list -> t -> t

  (** Construct as [remove] *)
  val with_remove: command list -> t -> t

  (** Construct as [libraries] *)
  val with_libraries: (string * filter option) list -> t -> t

  (** Replace the [syntax] field of the given OPAM file. *)
  val with_syntax: (string * filter option) list -> t -> t

  (** Construct as [substs] *)
  val with_substs: basename list -> t -> t

  val with_build_env: env_update list -> t -> t

  val with_available: filter -> t -> t

  (** Construct as [maintainer] *)
  val with_maintainer: string list -> t -> t

  val with_author: string list -> t -> t

  val with_homepage: string list -> t -> t

  val with_license: string list -> t -> t

  (** Construct as [patches] *)
  val with_patches: (basename * filter option) list -> t -> t

  (** Construct using [bug_reports] *)
  val with_bug_reports: string list -> t -> t

  (** Construct using [depexts] *)
  val with_depexts: (OpamSysPkg.Set.t * filter) list -> t -> t

  val with_flags: package_flag list -> t -> t

  val add_flags: package_flag list -> t -> t

  val with_tags: string list -> t -> t

  val with_env: env_update list -> t -> t

  val with_dev_repo: url -> t -> t

  val with_dev_repo_opt: url option -> t -> t

  val with_pin_depends: (package * url) list -> t -> t

  val with_extra_sources: (basename * URL.t) list -> t -> t

  val with_extensions: value OpamStd.String.Map.t -> t -> t

  val add_extension: t -> string -> value -> t

  val remove_extension: t -> string -> t

  val with_deprecated_build_doc: command list -> t -> t

  val with_deprecated_build_test: command list -> t -> t

  val with_descr: Descr.t -> t -> t
  val with_descr_opt: Descr.t option -> t -> t
  val with_synopsis: string -> t -> t

  (** If [synopsis] is not already set, split the string and use the first line
      as synopsis. *)
  val with_descr_body: string -> t -> t

  val with_url: URL.t -> t -> t
  val with_url_opt: URL.t option -> t -> t

  val with_metadata_dir: (repository_name option * string) option -> t -> t

  val with_extra_files: (OpamFilename.Base.t * OpamHash.t) list -> t -> t
  val with_extra_files_opt: (OpamFilename.Base.t * OpamHash.t) list option -> t -> t

  val with_locked_opt: string option -> t -> t

  val with_format_errors: (string * OpamPp.bad_format) list -> t -> t

  (** Prints to a string, while keeping the format of the original file as much
      as possible. The original format is read from the given
      [format_from_string], the file [format_from], or the output file if
      it exists *)
  val to_string_with_preserved_format:
    ?format_from:(t typed_file) -> ?format_from_string:string ->
    t typed_file -> t -> string

  (** Writes an opam file, but preserving the existing formatting as much as
      possible. The original format is read from the given
      [format_from_string], the file [format_from], or the output file if
      it exists *)
  val write_with_preserved_format:
    ?format_from:(t typed_file) -> ?format_from_string:string ->
    t typed_file -> t -> unit

  (** Low-level values used for linting and similar processing *)

  (** Allow 'flag:xxx' tags as flags, for compat *)
  val flag_of_tag: string -> package_flag option

  val fields: (t, value) OpamFormat.I.fields_def

  val sections:
    (t, (string option * opamfile_item list) list) OpamFormat.I.fields_def

  (** Doesn't handle package name encoded in directory name *)
  val pp_raw_fields: (opamfile_item list, t) OpamPp.t

  (** Returns the raw print-AST contents of the file *)
  val contents: ?filename:'a typed_file -> t -> opamfile

  (** Returns all fields of the file as print-AST. Fields within sections are
      accessed through dot-separated paths (e.g. [url.checksum]) *)
  val to_list: ?filename:'a typed_file -> t -> (string * value) list

  (** Gets the print-AST for a single field in the file structure. Fields within
      sections can be accessed through [section.field]. *)
  val print_field_as_syntax: string -> t -> value option

end

(** Compiler aliases: [$opam/aliases]. Deprecated, used only for migration *)
module Aliases: IO_FILE with type t = string switch_map

(** Switch state file as table, also used for import/export. This includes
    compiler and root packages information, as well as pinned packages and their
    target (but not their local metadata). *)
module LegacyState: sig
  type t = switch_selections
  include IO_FILE with type t := t
end

(** A newer format for switch state, using the opam file syntax rather than a
    table. This is more readable and extensible. *)
module SwitchSelections: sig
  type t = switch_selections
  include IO_FILE with type t := t
  module BestEffort: BestEffortRead with type t := t
end

(** An extended version of SwitchSelections that can include full opam files as
    [package "name" {}] sections, for storing overlays *)
module SwitchExport: sig
  type t = {
    selections: switch_selections;
    extra_files: string OpamHash.Map.t;
    overlays: OPAM.t OpamPackage.Name.Map.t;
  }
  include IO_FILE with type t := t
end

(** A simple list of packages and versions: (used for the older
    [$opam/$switch/{installed,installed_roots}], still needed to
    migrate from 1.2 repository, and for reinstall) *)
module PkgList: IO_FILE with type t = package_set

(** Cached environment updates (<switch>/environment) *)
module Environment: IO_FILE with type t = env_update list

(** Compiler version [$opam/compilers/]. Deprecated, only used to upgrade old
    data *)
module Comp: sig

  include IO_FILE

  type compiler = string
  type compiler_version = string

  (** Create a pre-installed compiler description file *)
  val create_preinstalled:
    compiler -> compiler_version -> name list -> env_update list -> t

  (** Is it a pre-installed compiler description file *)
  val preinstalled: t -> bool

  (** Get OPAM version *)
  val opam_version: t -> opam_version

  (** Return the compiler name *)
  val name: t -> compiler

  (** Return the compiler version *)
  val version: t -> compiler_version

  (** Return the url of the compiler *)
  val src: t -> url option

  (** Return the list of patches to apply *)
  val patches: t -> url list

  (** Options to give to the "./configure" command *)
  val configure: t -> string list

  (** Options to give to the "make" command *)
  val make: t -> string list

  (** Options to give to build the package. If this one is provided,
      nothing should be specified for [configure] and [make]. *)
  val build: t -> command list

  (** Packages to install immediately after the creation of OCaml *)
  val packages: t -> formula

  (** Environment variable to set-up before running commands in the
      subtree *)
  val env: t -> env_update list

  val tags: t -> string list

  val with_src: url option -> t -> t
  val with_patches: url list -> t -> t
  val with_configure: string list -> t -> t
  val with_make: string list -> t -> t
  val with_build: command list -> t -> t
  val with_packages: formula -> t -> t

  (** Converts a compiler definition to package metadata. For compat. If
      [package] is unspecified, a package named "ocaml" is created for
      "standard" compilers (when the compiler name doesn't contain a "+" and is
      equal to the compiler version); otherwise, a package "ocaml-VARIANT" is
      created with "VARIANT" the part of the compiler name on the right of the
      "+". In both case, the version corresponds to the OCaml version and is
      [version comp]. *)
  val to_package: ?package:package -> t -> Descr.t option -> OPAM.t

end

(** {2 Configuration files} *)

(** .install files *)
module Dot_install: sig

  include IO_FILE

  (** List of files to install in $bin/ *)
  val bin:  t -> (basename optional * basename option) list

  (** List of files to install in $sbin/ *)
  val sbin: t -> (basename optional * basename option) list

  (** List of files to install in $lib/ *)
  val lib:  t -> (basename optional * basename option) list

  (** List of toplevel files *)
  val toplevel: t -> (basename optional * basename option) list

  (** C bindings *)
  val stublibs: t -> (basename optional * basename option) list

  (** List of architecture-independent files *)
  val share: t -> (basename optional * basename option) list

  (** List of files under the more general share prefix *)
  val share_root: t -> (basename optional * basename option) list

  (** List of etc files *)
  val etc: t -> (basename optional * basename option) list

  (** List of doc files *)
  val doc: t -> (basename optional * basename option) list

  (** Man pages *)
  val man: t -> (basename optional * basename option) list

  (** Executable files under lib/ *)
  val libexec: t -> (basename optional * basename option) list

  (** Not relative to the package's lib dir *)
  val lib_root: t -> (basename optional * basename option) list

  (** Not relative to the package's lib dir, and with +x set *)
  val libexec_root: t -> (basename optional * basename option) list

  (** List of other files to install *)
  val misc: t -> (basename optional * filename) list

  (** List of files to install in $bin/ *)
  val with_bin : (basename optional * basename option) list -> t -> t

  (** List of files to install in $sbin/ *)
  val with_sbin : (basename optional * basename option) list -> t -> t

  (** List of files to install in $lib/ *)
  val with_lib : (basename optional * basename option) list -> t -> t

  (** List of toplevel files *)
  val with_toplevel : (basename optional * basename option) list -> t -> t

  (** C bindings *)
  val with_stublibs : (basename optional * basename option) list -> t -> t

  (** List of architecture-independent files *)
  val with_share : (basename optional * basename option) list -> t -> t

  (** List of files under the more general share prefix *)
  val with_share_root : (basename optional * basename option) list -> t -> t

  (** List of etc files *)
  val with_etc : (basename optional * basename option) list -> t -> t

  (** List of doc files *)
  val with_doc : (basename optional * basename option) list -> t -> t

  (** Man pages *)
  val with_man : (basename optional * basename option) list -> t -> t

  (** Executable files under lib/ *)
  val with_libexec : (basename optional * basename option) list -> t -> t

  (** Not relative to the package's lib dir *)
  val with_lib_root : (basename optional * basename option) list -> t -> t

  (** Not relative to the package's lib dir, and with +x set *)
  val with_libexec_root : (basename optional * basename option) list -> t -> t

  (** List of other files to install *)
  val with_misc : (basename optional * filename) list -> t -> t

end

(** .changes files, bound to the OpamDirTrack module *)
module Changes: sig
  type t = OpamDirTrack.t
  include IO_FILE with type t := t
end

(** .config files *)
module Dot_config: sig

  include IO_FILE

  (** Create a new .config file (containing only variables) *)
  val create: (variable * variable_contents) list -> t

  (** Dependency towards file-system paths and their hashes *)
  val file_depends: t -> (filename * OpamHash.t) list

  val with_file_depends: (filename * OpamHash.t) list -> t -> t

  (** Sets all bindings in the file *)
  val with_vars: (variable * variable_contents) list -> t -> t

  (** Top-level variables *)
  val variable: t -> variable  -> variable_contents option

  (** The list of top-level variables *)
  val variables: t -> variable list

  (** Lists all the variable bindings in the file *)
  val bindings: t -> (variable * variable_contents) list

  (** Sets the given variable, overriding any previous definition.
      With [None], unsets the variable*)
  val set: variable -> variable_contents option -> t -> t

end

(** {2 Repository files} *)

(** Association between package names and repositories *)
module Package_index: IO_FILE with
  type t = (repository_name * string option) package_map

(** Repository config: [$opam/repo/$repo/config]. Deprecated, for migration
    only *)
module Repo_config_legacy : sig
  type t = {
    repo_name : repository_name;
    repo_root : dirname;
    repo_url : url;
    repo_priority : int;
  }
  include IO_FILE with type t := t
end

module Repos_config: sig
  type t = (url * trust_anchors option) option OpamRepositoryName.Map.t
  include IO_FILE with type t := t
  module BestEffort: BestEffortRead with type t := t
end

module Switch_config: sig
  type t = {
    opam_version: OpamVersion.t;
    synopsis: string;
    repos: repository_name list option;
    paths: (std_path * string) list;
    variables: (variable * variable_contents) list;
    opam_root: dirname option;
    wrappers: Wrappers.t;
    env: env_update list;
    invariant: OpamFormula.t option;
    depext_bypass: OpamSysPkg.Set.t;
  }
  val file_format_version: OpamVersion.t
  val variable: t -> variable -> variable_contents option
  val path: t -> std_path -> string option
  val wrappers: t -> Wrappers.t
  val sections: (string * (t, (string option * opamfile_item list) list) OpamPp.field_parser) list
  val fields: (string * (t, value) OpamPp.field_parser) list
  val to_list: ?filename:'a typed_file -> t -> (string * value) list
  include IO_FILE with type t := t
  val oldest_compatible_format_version: OpamVersion.t

  module BestEffort: BestEffortRead with type t := t
end

(** Pinned package files (only used for migration from 1.2, the inclusive State
    module is now used instead) *)
module Pinned_legacy: sig
  type pin_option =
    | Version of version
    | Source of url
  include IO_FILE with type t = pin_option name_map
end

(** Repository metadata *)
module Repo: sig

  include IO_FILE

  val create:
    ?browse:string -> ?upstream:string -> ?opam_version:OpamVersion.t ->
    ?redirect:(string * filter option) list -> ?root_url:url ->
    ?dl_cache:string list -> ?announce:(string * filter option) list ->
    ?stamp:string ->
    unit -> t

  (** The minimum OPAM version required for this repository, if defined *)
  val opam_version : t -> OpamVersion.t option

  (** Base URL for browsing packages on the WWW *)
  val browse: t -> string option

  (** Base URL for browsing OPAM repository source on the WWW *)
  val upstream: t -> string option

  (** The root URL of the repository (not an actual file field, determined at
      runtime by opam) *)
  val root_url: t -> url option

  (** Redirections. *)
  val redirect: t -> (string * filter option) list

  (** Cache URLs, either full or relative to the repo root *)
  val dl_cache: t -> string list

  val announce: t -> (string * filter option) list

  val stamp: t -> string option

  val with_opam_version : OpamVersion.t -> t -> t

  val with_browse: string -> t -> t

  val with_upstream: string -> t -> t

  val with_redirect: (string * filter option) list -> t -> t

  val with_root_url: url -> t -> t

  val with_dl_cache: string list -> t -> t

  val with_announce: (string * filter option) list -> t -> t

  val with_stamp: string -> t -> t

  val with_stamp_opt: string option -> t -> t
end

(** {2 urls.txt file *} *)
module File_attributes: IO_FILE with type t = file_attribute_set

module Stats: sig

  (** Display statistics about file access. *)
  val print: unit -> unit

end

(** Helper module for manipulation of the raw syntax ([opamfile]) format.
    (the specific file handling modules are derived from this one) *)
module Syntax : sig

  val pp_channel:
    'a typed_file -> in_channel -> out_channel ->
    (unit, opamfile) OpamPp.t

  val of_channel: 'a typed_file -> in_channel  -> opamfile
  val to_channel: 'a typed_file -> out_channel -> opamfile -> unit
  val of_string: 'a typed_file -> string -> opamfile
  val to_string: 'a typed_file -> opamfile -> string
  val to_string_with_preserved_format:
    'a typed_file -> ?format_from:'a typed_file -> ?format_from_string:string ->
    empty:'a ->
    ?sections:('a, (string option * opamfile_item list) list)
      OpamFormat.I.fields_def ->
    fields:('a, value) OpamFormat.I.fields_def ->
    (opamfile, filename * 'a) OpamPp.t ->
    'a -> string

end

(**/**)

module type SyntaxFileArg = sig
  val internal: string
  val atomic: bool
  val format_version: OpamVersion.t
  type t
  val empty: t
  val pp: (opamfile, filename * t) OpamPp.t
end

module SyntaxFile(X: SyntaxFileArg) : IO_FILE with type t := X.t

module type LineFileArg = sig
  val internal: string
  val atomic: bool
  type t
  val empty: t
  val pp: (string list list, t) OpamPp.t
end

module LineFile (X: LineFileArg) : IO_FILE with type t := X.t
