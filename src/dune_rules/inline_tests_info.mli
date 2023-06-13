open Import

module Backend : sig
  type t =
    { loc : Loc.t
    ; runner_libraries : (Loc.t * Lib_name.t) list
    ; flags : Ordered_set_lang.Unexpanded.t
    ; list_partitions_flags : Ordered_set_lang.Unexpanded.t option
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

  val to_string : t -> string

  module Map : Map.S with type key = t

  module Set : sig
    include Set.S with type elt = t and type 'a map = 'a Map.t

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
    ; node_flags : Ordered_set_lang.Unexpanded.t
    ; executable_ocaml_flags : Ocaml_flags.Spec.t
    ; executable_link_flags : Ordered_set_lang.Unexpanded.t
    ; backend : (Loc.t * Lib_name.t) option
    ; libraries : (Loc.t * Lib_name.t) list
    ; enabled_if : Blang.t
    }

  val backends : t -> (Loc.t * Lib_name.t) list option

  include Sub_system_info.S with type t := t
end
