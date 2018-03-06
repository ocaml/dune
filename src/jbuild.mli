(** Representation and parsing of jbuild files *)

open Import

module Jbuild_version : sig
  type t = V1
  val t : t Sexp.Of_sexp.t

  val latest_stable : t
end

module Scope_info : sig
  module Name : sig
    (* CR-someday diml: change to [private string] and encode [None]
       as [""] *)
    (** [None] is the for the {!anonymous} scope *)
    type t = string option

    val compare : t -> t -> Ordering.t

    val of_string : string -> t
    val to_string : t -> string
  end

  type t =
    { name     : string option (** First package name in alphabetical
                                   order. [None] for the global
                                   scope. *)
    ; packages : Package.t Package.Name.Map.t
    ; root     : Path.t
    }

  val make : Package.t list -> t

  (** The anonymous represent the scope at the root of the workspace
      when the root of the workspace contains no [<package>.opam]
      files. *)
  val anonymous : t

  (** [resolve t package_name] looks up [package_name] in [t] and returns the
      package description if it exists, otherwise it returns an error. *)
  val resolve : t -> Package.Name.t -> (Package.t, string) result
end

(** Ppx preprocessors  *)
module Pp : sig
  type t = private string
  val of_string : string -> t
  val to_string : t -> string
end

module Preprocess : sig
  type pps =
    { pps   : (Loc.t * Pp.t) list
    ; flags : string list
    }

  type t =
    | No_preprocessing
    | Action of Action.Unexpanded.t
    | Pps    of pps
end

module Per_module : Per_item.S with type key = Module.Name.t

module Preprocess_map : sig
  type t = Preprocess.t Per_module.t

  val no_preprocessing : t
  val default : t

  (** [find module_name] find the preprocessing specification for a
      given module *)
  val find : Module.Name.t -> t -> Preprocess.t

  val pps : t -> (Loc.t * Pp.t) list
end

module Lint : sig
  type t = Preprocess_map.t

  val no_lint : t
end


module Js_of_ocaml : sig
  type t =
    { flags            : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  val default : t
end

module Lib_dep : sig
  type choice =
    { required  : String_set.t
    ; forbidden : String_set.t
    ; file      : string
    }

  type select =
    { result_fn : string
    ; choices   : choice list
    ; loc       : Loc.t
    }

  type t =
    | Direct of (Loc.t * string)
    | Select of select

  val to_lib_names : t -> string list
  val direct : Loc.t * string -> t
  val of_pp : Loc.t * Pp.t -> t
end

module Lib_deps : sig
  type t = Lib_dep.t list
end

module Dep_conf : sig
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Alias_rec of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Files_recursively_in of String_with_vars.t

  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t -> Sexp.t
end

module Buildable : sig
  type t =
    { loc                      : Loc.t
    ; modules                  : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries                : Lib_dep.t list
    ; preprocess               : Preprocess_map.t
    ; preprocessor_deps        : Dep_conf.t list
    ; lint                     : Lint.t
    ; flags                    : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags             : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags           : Ordered_set_lang.Unexpanded.t
    ; js_of_ocaml              : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : t -> Preprocess.t
end

module Public_lib : sig
  type t =
    { name    : string        (** Full public name *)
    ; package : Package.t     (** Package it is part of *)
    ; sub_dir : string option (** Subdirectory inside the installation
                                  directory *)
    }
end

module Sub_system_info : sig
  (** The type of all sub-systems informations. This type is what we
      get just after parsing a [jbuild] file. *)
  type t = ..
  type sub_system = t = ..

  type 'a parser =
    { short : (Loc.t -> 'a) option (** Value when the sub-system has
                                       no argument *)
    ; parse : 'a Sexp.Of_sexp.t    (** Parse the argument *)
    }

  (** What the user must provide in order to define the parsing part
      of a sub-system. *)
  module type S = sig
    type t
    type sub_system += T of t

    (** Name of the sub-system *)
    val name : Sub_system_name.t

    (** Location of the S-expression passed to [of_sexp] or [short]. *)
    val loc : t -> Loc.t

    val parsers : t parser Syntax.Versioned_parser.t
  end

  module Register(M : S) : sig end

  val get : Sub_system_name.t -> (module S)
end

module Library : sig
  module Kind : sig
    type t =
      | Normal
      | Ppx_deriver
      | Ppx_rewriter
  end

  type t =
    { name                     : string
    ; public                   : Public_lib.t option
    ; synopsis                 : string option
    ; install_c_headers        : string list
    ; ppx_runtime_libraries    : (Loc.t * string) list
    ; modes                    : Mode.Dict.Set.t
    ; kind                     : Kind.t
    ; c_flags                  : Ordered_set_lang.Unexpanded.t
    ; c_names                  : string list
    ; cxx_flags                : Ordered_set_lang.Unexpanded.t
    ; cxx_names                : string list
    ; library_flags            : Ordered_set_lang.Unexpanded.t
    ; c_library_flags          : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps             : (Loc.t * string) list
    ; wrapped                  : bool
    ; optional                 : bool
    ; buildable                : Buildable.t
    ; dynlink                  : bool
    ; scope_name               : Scope_info.Name.t
    ; sub_systems              : Sub_system_info.t Sub_system_name.Map.t
    }

  val has_stubs : t -> bool
  val stubs_archive : t -> dir:Path.t -> ext_lib:string -> Path.t
  val best_name : t -> string
end

module Install_conf : sig
  type file =
    { src : string
    ; dst : string option
    }

  type t =
    { section : Install.Section.t
    ; files   : file list
    ; package : Package.t
    }
end

module Executables : sig
  type t =
    { names            : (Loc.t * string) list
    ; link_executables : bool
    ; link_flags       : Ordered_set_lang.Unexpanded.t
    ; modes            : Mode.Dict.Set.t
    ; buildable        : Buildable.t
    }
end

module Rule : sig
  module Targets : sig
    type t =
      | Static of string list
      | Infer
  end

  module Mode : sig
    type t =
      | Standard
      (** Only use this rule if  the source files don't exist. *)
      | Fallback
      (** Silently promote the targets to the source tree. *)
      | Promote
      (** Same as [Promote] but [jbuilder clean] must delete the file *)
      | Promote_but_delete_on_clean
      (** Same as [Standard] however this is not a rule stanza, so it is not possible to
          add a [(fallback)] field to the rule. *)
      | Not_a_rule_stanza
      (** Just ignore the source files entirely. This is for cases where the targets are
          promoted only in a specific context, such as for .install files. *)
      | Ignore_source_files
  end

  type t =
    { targets  : Targets.t
    ; deps     : Dep_conf.t list
    ; action   : Action.Unexpanded.t
    ; mode     : Mode.t
    ; locks    : String_with_vars.t list
    ; loc      : Loc.t
    }
end

module Menhir : sig
  type t =
    { merge_into : string option
    ; flags      : Ordered_set_lang.Unexpanded.t
    ; modules    : string list
    ; mode       : Rule.Mode.t
    ; loc        : Loc.t
    }
end

module Provides : sig
  type t =
    { name : string
    ; file : string
    }
end

module Alias_conf : sig
  type t =
    { name    : string
    ; deps    : Dep_conf.t list
    ; action  : Action.Unexpanded.t option
    ; locks   : String_with_vars.t list
    ; package : Package.t option
    }
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; glob : String_with_vars.t
    }
end

module Stanza : sig
  type t =
    | Library     of Library.t
    | Executables of Executables.t
    | Rule        of Rule.t
    | Provides    of Provides.t
    | Install     of Install_conf.t
    | Alias       of Alias_conf.t
    | Copy_files  of Copy_files.t
    | Menhir      of Menhir.t
end

module Stanzas : sig
  type t = Stanza.t list

  type syntax = OCaml | Plain

  val parse
    :  ?default_version:Jbuild_version.t
    -> file:Path.t
    -> Scope_info.t
    -> Sexp.Ast.t list
    -> t
  val lib_names : (_ * _ * t) list -> String_set.t
end
