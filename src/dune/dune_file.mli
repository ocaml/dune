(** Representation and parsing of jbuild files *)

open! Stdune
open Import

module Preprocess : sig
  module Pps : sig
    type t =
      { loc : Loc.t
      ; pps : (Loc.t * Lib_name.t) list
      ; flags : String_with_vars.t list
      ; staged : bool
      }

    val compare_no_locs : t -> t -> Ordering.t
  end

  type t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of Pps.t
    | Future_syntax of Loc.t

  module Without_future_syntax : sig
    type t =
      | No_preprocessing
      | Action of Loc.t * Action_dune_lang.t
      | Pps of Pps.t
  end

  val loc : t -> Loc.t option

  module Pp_flag_consumer : sig
    type t =
      | Compiler
      | Merlin
  end

  val remove_future_syntax :
    t -> for_:Pp_flag_consumer.t -> Ocaml_version.t -> Without_future_syntax.t
end

module Per_module : Per_item.S with type key = Module_name.t

module Preprocess_map : sig
  type t = Preprocess.t Per_module.t

  val decode : t Dune_lang.Decoder.t

  val no_preprocessing : t

  val default : t

  (** [find module_name] find the preprocessing specification for a given
      module *)
  val find : Module_name.t -> t -> Preprocess.t

  val pps : t -> (Loc.t * Lib_name.t) list
end

module Lint : sig
  type t = Preprocess_map.t

  val no_lint : t
end

module Js_of_ocaml : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  val default : t
end

module Lib_deps : sig
  type nonrec t = Lib_dep.t list

  val of_pps : Lib_name.t list -> t

  val info : t -> kind:Lib_deps_info.Kind.t -> Lib_deps_info.t

  val decode : allow_re_export:bool -> t Dune_lang.Decoder.t
end

module Dep_conf : sig
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Alias_rec of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Source_tree of String_with_vars.t
    | Package of String_with_vars.t
    | Universe
    | Env_var of String_with_vars.t
    (* [Sandbox_config] is a way to declare that your action also depends on
       there being a clean filesystem around its deps. (or, if you require
       [no_sandboxing], it's that your action depends on something undeclared
       (e.g. absolute path of cwd) and you want to allow it) *)
    | Sandbox_config of Sandbox_config.t

  val remove_locs : t -> t

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t Dyn.Encoder.t
end

module Buildable : sig
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; c_flags : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; c_names : Ordered_set_lang.t option
    ; cxx_names : Ordered_set_lang.t option
    ; preprocess : Preprocess_map.t
    ; preprocessor_deps : Loc.t * Dep_conf.t list
    ; lint : Lint.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : t -> Preprocess.t
end

module Public_lib : sig
  type t =
    { name : Loc.t * Lib_name.t  (** Full public name *)
    ; package : Package.t  (** Package it is part of *)
    ; sub_dir : string option
          (** Subdirectory inside the installation directory *)
    }

  val name : t -> Lib_name.t
end

module Mode_conf : sig
  type t =
    | Byte
    | Native
    | Best  (** [Native] if available and [Byte] if not *)

  val decode : t Dune_lang.Decoder.t

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  module Kind : sig
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map : sig
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }
  end

  module Set : sig
    type nonrec t = Kind.t option Map.t

    val decode : t Dune_lang.Decoder.t

    (** Byte inherited, Best is requested *)
    val default : t

    val eval : t -> has_native:bool -> Mode.Dict.Set.t
  end
end

module External_variant : sig
  type t =
    { implementation : Loc.t * Lib_name.t
    ; virtual_lib : Loc.t * Lib_name.t
    ; variant : Variant.t
    ; project : Dune_project.t
    ; loc : Loc.t
    }
end

module Library : sig
  module Inherited : sig
    type 'a t =
      | This of 'a
      | From of (Loc.t * Lib_name.t)
  end

  module Stdlib : sig
    (** Extra information for the OCaml stdlib. Note: contrary to normal
        libraries, the library interface of the stdlib (the Stdlib module) is
        used as the alias module when compiling all the other modules. We
        cannot generate an implicit one as that would break hard-coded names
        inside the compiler. *)
    type t =
      { modules_before_stdlib : Module_name.Set.t
            (** Modules that the Stdlib module depend on. *)
      ; exit_module : Module_name.t option
            (** Modules that's implicitely added by the compiler at the end
                when linking an executable *)
      ; internal_modules : Glob.t
            (** Module names that are hardcoded in the compiler and so cannot
                be wrapped *)
      }
  end

  module Special_builtin_support : sig
    module Build_info : sig
      type api_version = V1

      type t =
        { data_module : string
        ; api_version : api_version
        }
    end

    type t =
      | Findlib_dynload
      | Build_info of Build_info.t

    include Dune_lang.Conv.S with type t := t
  end

  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Set.t
    ; kind : Lib_kind.t
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; no_keep_locs : bool
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; variant : Variant.t option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Stdlib.t option
    ; special_builtin_support : Special_builtin_support.t option
    ; enabled_if : Blang.t
    }

  val has_stubs : t -> bool

  val stubs_name : t -> string

  val stubs : t -> dir:Path.Build.t -> Path.Build.t

  val stubs_archive : t -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t

  val dll : t -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t

  val archive : t -> dir:Path.Build.t -> ext:string -> Path.Build.t

  val best_name : t -> Lib_name.t

  val is_virtual : t -> bool

  val is_impl : t -> bool

  val obj_dir : dir:Path.Build.t -> t -> Path.Build.t Obj_dir.t

  module Main_module_name : sig
    type t = Module_name.t option Inherited.t
  end

  val main_module_name : t -> Main_module_name.t
end

module Install_conf : sig
  type 'file t =
    { section : Install.Section.t
    ; files : 'file list
    ; package : Package.t
    }
end

module Promote : sig
  module Lifetime : sig
    type t =
      | Unlimited  (** The promoted file will be deleted by [dune clean] *)
      | Until_clean
  end

  module Into : sig
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.t option
    }
end

module Executables : sig
  module Link_mode : sig
    type t =
      { mode : Mode_conf.t
      ; kind : Binary_kind.t
      ; loc : Loc.t
      }

    include Dune_lang.Conv.S with type t := t

    val exe : t

    val object_ : t

    val shared_object : t

    val byte : t

    val native : t

    val byte_exe : t

    val js : t

    val compare : t -> t -> Ordering.t

    val to_dyn : t -> Dyn.t

    module Set : Set.S with type elt = t
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps : Dep_conf.t list
    ; modes : Link_mode.Set.t
    ; optional : bool
    ; buildable : Buildable.t
    ; variants : (Loc.t * Variant.Set.t) option
    ; package : Package.t option
    ; promote : Promote.t option
    ; install_conf : File_binding.Unexpanded.t Install_conf.t option
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    }

  val has_stubs : t -> bool

  val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
end

module Rule : sig
  module Targets : sig
    module Multiplicity : sig
      type t =
        | One
        | Multiple
    end

    type static =
      { targets : String_with_vars.t list
      ; multiplicity : Multiplicity.t
      }

    type t =
      | Static of static
      | Infer
  end

  module Mode : sig
    type t =
      | Standard  (** Only use this rule if the source files don't exist. *)
      | Fallback  (** Silently promote the targets to the source tree. *)
      | Promote of Promote.t
          (** Just ignore the source files entirely. This is for cases where
              the targets are promoted only in a specific context, such as for
              .install files. *)
      | Ignore_source_files

    val decode : t Dune_lang.Decoder.t
  end

  type t =
    { targets : Targets.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Action_dune_lang.t
    ; mode : Mode.t
    ; locks : String_with_vars.t list
    ; loc : Loc.t
    ; enabled_if : Blang.t
    }
end

module Menhir : sig
  type t =
    { merge_into : string option
    ; flags : Ordered_set_lang.Unexpanded.t
    ; modules : string list
    ; mode : Rule.Mode.t
    ; loc : Loc.t
    ; infer : bool
    ; enabled_if : Blang.t
    }

  type Stanza.t += T of t
end

module Coq : sig
  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; libraries : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; loc : Loc.t
    ; enabled_if : Blang.t
    }

  val best_name : t -> Lib_name.t

  type Stanza.t += T of t
end

module Coqpp : sig
  type t =
    { modules : string list
    ; loc : Loc.t
    }

  type Stanza.t += T of t
end

module Alias_conf : sig
  type t =
    { name : string
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Action_dune_lang.t) option
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; glob : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }
end

module Documentation : sig
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }
end

module Tests : sig
  type t =
    { exes : Executables.t
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Action_dune_lang.t option
    }
end

module Toplevel : sig
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; variants : (Loc.t * Variant.Set.t) option
    ; loc : Loc.t
    }
end

module Include_subdirs : sig
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification
end

type Stanza.t +=
  | Library of Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of File_binding.Unexpanded.t Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | External_variant of External_variant.t

val stanza_package : Stanza.t -> Package.t option

module Stanzas : sig
  type t = Stanza.t list

  type syntax =
    | OCaml
    | Plain

  (** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
      [ast] according to the syntax given by [kind] in the context of the
      [project] *)
  val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

  (** [parse ~file ~kind project stanza_exprs] is a list of [Stanza.t]s derived
      from decoding the [stanza_exprs] from [Dune_lang.Ast.t]s to [Stanza.t]s.

      [file] is used to check for illegal recursive file inclusions and to
      anchor file includes given as relative paths.

      The stanzas are parsed in the context of the dune [project].

      The syntax [kind] determines whether the expected syntax is the
      depreciated jbuilder syntax or the version of dune syntax specified by
      the current [project]. *)
  val parse : file:Path.Source.t -> Dune_project.t -> Dune_lang.Ast.t list -> t
end
