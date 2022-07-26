open Import

module Build_flags_resolver : sig
  module Vendored : sig
    type t =
      { c_flags : Ordered_set_lang.Unexpanded.t
      ; c_library_flags : Ordered_set_lang.Unexpanded.t
      }
  end

  type t =
    | Pkg_config
    | Vendored of Vendored.t
end

module Concurrency_policy : sig
  type t =
    | Sequential
    | Unlocked
    | Lwt_jobs
    | Lwt_preemptive
end

module Errno_policy : sig
  type t =
    | Ignore_errno
    | Return_errno
end

module Headers : sig
  type t =
    | Include of Ordered_set_lang.Unexpanded.t
    | Preamble of String_with_vars.t
end

module Type_description : sig
  type t =
    { functor_ : Module_name.t
    ; instance : Module_name.t
    }
end

module Function_description : sig
  type t =
    { concurrency : Concurrency_policy.t
    ; errno_policy : Errno_policy.t
    ; functor_ : Module_name.t
    ; instance : Module_name.t
    }
end

type t =
  { external_library_name : External_lib_name.t
  ; build_flags_resolver : Build_flags_resolver.t
  ; headers : Headers.t
  ; type_description : Type_description.t
  ; function_description : Function_description.t list
  ; generated_types : Module_name.t
  ; generated_entry_point : Module_name.t
  ; deps : Dep_conf.t list
  }

type Stanza.t += T of t

val decode : t Dune_lang.Decoder.t

val syntax : Dune_lang.Syntax.t

val ml_of_module_name : Module_name.t -> string

val non_installable_modules : t -> Module_name.t list

val generated_ml_and_c_files : t -> string list

val c_generated_functions_module : t -> Function_description.t -> Module_name.t

val lib_deps_of_strings : loc:Loc.t -> string list -> Lib_dep.t list

val c_generated_types_module : t -> Module_name.t

val c_library_flags_sexp : t -> string

val cflags_sexp : t -> string

val type_gen_script_module : t -> Module_name.t

val type_gen_script : t -> string

val c_generated_functions_cout_c : t -> Function_description.t -> string

val function_gen_script : t -> Function_description.t -> string
