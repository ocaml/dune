(** Representation and parsing of jbuild files *)

open Import

module Jbuild_version : sig
  type t = V1
  val t : t Sexp.Of_sexp.t

  val latest_stable : t
end

module Scope : sig
  type t =
    { name     : string option (** First package name in alphabetical order.  [None] for
                                   the global scope. *)
    ; packages : Package.t String_map.t
    ; root     : Path.t
    }

  val make : Package.t list -> t

  val empty : t

  (** [resolve t package_name] looks up [package_name] in [t] and returns the
      package description if it exists, otherwise it returns an error. *)
  val resolve : t -> string -> (Package.t, string) result
end

(** Ppx preprocessors  *)
module Pp : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module Preprocess : sig
  type pps =
    { pps   : Pp.t list
    ; flags : string list
    }

  type t =
    | No_preprocessing
    | Action of Action.Unexpanded.t
    | Pps    of pps
end

module Preprocess_map : sig
  type t

  val no_preprocessing : t
  val default : t

  (** [find module_name] find the preprocessing specification for a given module *)
  val find : string -> t -> Preprocess.t

  val pps : t -> Pp.t list
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
    | Direct of string
    | Select of select

  val to_lib_names : t -> string list
  val direct : string -> t
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

  val sexp_of_t : t -> Sexp.t
end

module Buildable : sig
  type t =
    { modules                  : Ordered_set_lang.t
    ; libraries                : Lib_dep.t list
    ; preprocess               : Preprocess_map.t
    ; preprocessor_deps        : Dep_conf.t list
    ; lint                     : Lint.t
    ; flags                    : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags             : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags           : Ordered_set_lang.Unexpanded.t
    ; js_of_ocaml              : Js_of_ocaml.t
    ; (** [true] except for on-demand utops, to avoid generation
          [.utop/.merlin] files everywhere. *)
      gen_dot_merlin           : bool
    }

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : t -> Preprocess.t
end

module Public_lib : sig
  type t =
    { name    : string        (** Full public name *)
    ; package : Package.t     (** Package it is part of *)
    ; sub_dir : string option (** Subdirectory inside the installation directory *)
    }
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
    ; ppx_runtime_libraries    : string list
    ; modes                    : Mode.Dict.Set.t
    ; kind                     : Kind.t
    ; c_flags                  : Ordered_set_lang.Unexpanded.t
    ; c_names                  : string list
    ; cxx_flags                : Ordered_set_lang.Unexpanded.t
    ; cxx_names                : string list
    ; library_flags            : Ordered_set_lang.Unexpanded.t
    ; c_library_flags          : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps             : string list
    ; wrapped                  : bool
    ; optional                 : bool
    ; buildable                : Buildable.t
    ; dynlink                  : bool
    }

  val has_stubs : t -> bool
  val stubs_archive : t -> dir:Path.t -> ext_lib:string -> Path.t
  val all_lib_deps : t -> Lib_deps.t
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
    { names            : string list
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

  module Fallback : sig
    type t =
      | Yes
      | No
      | Not_possible
      (** It is not possible to add a [(fallback)] field to the rule. For instance for
          [ocamllex], ... *)
  end

  type t =
    { targets  : Targets.t
    ; deps     : Dep_conf.t list
    ; action   : Action.Unexpanded.t
    ; fallback : Fallback.t
    ; locks    : String_with_vars.t list
    ; loc      : Loc.t
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
end

module Stanzas : sig
  type t = Stanza.t list

  val parse : Scope.t -> Sexp.Ast.t list -> t
  val lib_names : (_ * _ * t) list -> String_set.t
end
