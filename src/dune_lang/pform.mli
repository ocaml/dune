open Stdune
open Dune_sexp
module Payload = Template.Pform.Payload

module Var : sig
  module Pkg : sig
    module Section : sig
      type t =
        | Lib
        | Libexec
        | Bin
        | Sbin
        | Toplevel
        | Share
        | Etc
        | Doc
        | Stublibs
        | Man

      val of_string : string -> t option
    end

    type t =
      | Switch
      | Os
      | Os_version
      | Os_distribution
      | Os_family
      | Build
      | Prefix
      | User
      | Group
      | Jobs
      | Arch
      | Sys_ocaml_version
      | Section_dir of Section.t

    val compare : t -> t -> Ordering.t
    val to_dyn : t -> Dyn.t
  end

  type t =
    | User_var of string
    | Nothing
    | Project_root
    | Workspace_root
    | First_dep
    | Deps
    | Targets
    | Target
    | Cc
    | Cxx
    | Ccomp_type
    | Cpp
    | Pa_cpp
    | Make
    | Ocaml_version
    | Ocaml
    | Ocamlc
    | Ocamlopt
    | Ocaml_bin_dir
    | Ocaml_stdlib_dir
    | Dev_null
    | Ext_obj
    | Ext_asm
    | Ext_lib
    | Ext_dll
    | Ext_exe
    | Ext_plugin
    | Profile
    | Context_name
    | Os_type
    | Architecture
    | Arch_sixtyfour
    | System
    | Model
    | Ignoring_promoted_rules
    | Input_file
    | Library_name
    | Partition
    | Impl_files
    | Intf_files
    | Test
    | Corrected_suffix
    | Inline_tests
    | Toolchain
    | Pkg of Pkg.t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t

  (** Translate a global variable from an opam file into the equivalent dune
      pform variable. *)
  val of_opam_global_variable_name : string -> t option
end

module Artifact : sig
  type t =
    | Mod of Ocaml.Cm_kind.t
    | Lib of Ocaml.Mode.t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
  val ext : t -> string
  val all : t list
end

module Macro : sig
  type t =
    | Exe
    | Dep
    | Bin
    (* All four combinations are allowed and correspond to variables [lib],
       [libexec], [lib-private], and [libexec-private]. *)
    | Lib of
        { lib_exec : bool
        ; lib_private : bool
        }
    | Lib_available
    | Bin_available
    | Version
    | Read
    | Read_strings
    | Read_lines
    | Path_no_dep
    | Ocaml_config
    | Coq_config
    | Env
    | Artifact of Artifact.t
    | Pkg
    | Pkg_self

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

module Macro_invocation : sig
  type t =
    { macro : Macro.t
    ; payload : Payload.t
    }

  (** Similar to [Payload.Args] but incorporates the macro name into error messages *)
  module Args : sig
    (** Treat the entire payload as a single string argument. *)
    val whole : t -> string

    (** Split the payload on the first ':' character raising a [User_error] if the
        payload contains no ':' characters. *)
    val lsplit2_exn : t -> Loc.t -> string * string

    (** Split the payload on all ':' characters *)
    val split : t -> string list
  end
end

type t =
  | Var of Var.t
  | Macro of Macro_invocation.t

val compare : t -> t -> Ordering.t
val to_dyn : t -> Dyn.t

type encode_result =
  | Success of
      { name : string
      ; payload : Payload.t option
      }
  | Pform_was_deleted

(** Enode using the latest name for a given percent form. *)
val encode_to_latest_dune_lang_version : t -> encode_result

(** "macro" or "variable" *)
val describe_kind : t -> string

module Map : Map.S with type key = t

module Env : sig
  type pform := t

  (** Decoding environment *)
  type t

  val pkg : Syntax.Version.t -> t
  val initial : Syntax.Version.t -> t
  val add_user_vars : t -> string list -> t
  val parse : t -> Template.Pform.t -> pform

  (** Used to parse percent forms in [enabled_if] fields, as the checks are done
      manually *)
  val unsafe_parse_without_checking_version : t -> Template.Pform.t -> pform

  (** The set of all variables and macros known, including deleted ones. Macros
      are returned with an empty payload. *)
  val all_known : t -> pform String.Map.t

  val syntax_version : t -> Syntax.Version.t

  (** Introduce the renaming "<" -> "input-file". In [initial], "<" is marked as
      deleted. *)
  val lt_renamed_input_file : t -> t

  type stamp

  val to_stamp : t -> stamp
  val to_dyn : t -> Dyn.t
end
