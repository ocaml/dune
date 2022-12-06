open Import

(** Stanza to produce JavaScript targets from Melange libraries *)
module Emit : sig
  type t =
    { loc : Loc.t
    ; target : string
    ; alias : Alias.Name.t option
    ; module_system : Melange.Module_system.t
    ; entries : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; promote : Rule.Promote.t option
    ; compile_flags : Ordered_set_lang.Unexpanded.t
    ; root_module : (Loc.t * Module_name.t) option
    ; javascript_extension : string
    }

  val decode : t Dune_lang.Decoder.t
end
