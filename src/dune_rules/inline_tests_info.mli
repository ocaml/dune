open! Dune_engine
open! Stdune
open! Import

module Backend : sig
  type t =
    { loc : Loc.t
    ; runner_libraries : (Loc.t * Lib_name.t) list
    ; flags : Ordered_set_lang.Unexpanded.t
    ; generate_runner : (Loc.t * Action_unexpanded.t) option
    ; extends : (Loc.t * Lib_name.t) list
    }

  include Sub_system_info.S with type t := t
end

module Mode_conf : sig
  type t =
    | Byte
    | Javascript
    | Native
    | Best

  val compare : t -> t -> ordering

  val to_dyn : t -> Dyn.t

  val decode : t Dune_lang.Decoder.t

  module Set : sig
    include Set.S with type elt = t

    val decode : t Dune_lang.Decoder.t

    val default : t
  end
end

module Tests : sig
  type t =
    { loc : Loc.t
    ; deps : Dep_conf.t list
    ; modes : Mode_conf.Set.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; executable : Ocaml_flags.Spec.t
    ; backend : (Loc.t * Lib_name.t) option
    ; libraries : (Loc.t * Lib_name.t) list
    }

  val backends : t -> (Loc.t * Lib_name.t) list option

  include Sub_system_info.S with type t := t
end
