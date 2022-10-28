open Import

val syntax : Dune_lang.Syntax.t

val extension_key : unit Dune_engine.Dune_project.Extension.t

val js_ext : string

module Spec : sig
  type t =
    | Es6
    | CommonJs

  val to_string : t -> string
end

module In_context : sig
  type t =
    { lib_rel_path : string
    ; pkg_name : string
    ; spec : Spec.t
    }

  val make : lib_rel_path:string -> pkg_name:string -> spec:Spec.t -> t
end

module Cm_kind : sig
  type t =
    | Cmi
    | Cmj

  val source : t -> Ocaml.Ml_kind.t

  val ext : t -> string

  val to_dyn : t -> Dyn.t

  module Map : sig
    type 'a t =
      { cmi : 'a
      ; cmj : 'a
      }

    val make_all : 'a -> 'a t
  end
end

val lib_output_dir :
     melange_stanza_dir:Path.Build.t
  -> lib_dir:Path.Build.t
  -> target:string
  -> Path.Build.t
