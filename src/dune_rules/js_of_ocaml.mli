open Import

module Ext : sig
  type t = string

  val exe : t

  val cmo : t

  val cma : t

  val runtime : t
end

module Flags : sig
  type 'flags t =
    { build_runtime : 'flags
    ; compile : 'flags
    ; link : 'flags
    }

  module Spec : sig
    type nonrec t = Ordered_set_lang.Unexpanded.t t
  end

  val build_runtime : 'a t -> 'a

  val compile : 'a t -> 'a

  val link : 'a t -> 'a

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val standard : Spec.t

  val make :
       spec:Spec.t
    -> default:string list Action_builder.t t
    -> eval:
         (   Ordered_set_lang.Unexpanded.t
          -> standard:string list Action_builder.t
          -> string list Action_builder.t)
    -> string list Action_builder.t t

  val dump : string list Action_builder.t t -> Dune_lang.t list Action_builder.t
end

module In_buildable : sig
  type t =
    { flags : Flags.Spec.t
    ; javascript_files : string list
    }

  val decode : t Dune_lang.Decoder.t

  val default : t
end

module In_context : sig
  type t =
    { flags : Flags.Spec.t
    ; javascript_files : Path.Build.t list
    }

  val make : dir:Path.Build.t -> In_buildable.t -> t

  val default : t
end

module Compilation_mode : sig
  type t =
    | Whole_program
    | Separate_compilation
end

module Env : sig
  type 'a t =
    { compilation_mode : Compilation_mode.t option
    ; runtest_alias : Alias.Name.t option
    ; flags : 'a Flags.t
    }

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val equal :
    Ordered_set_lang.Unexpanded.t t -> Ordered_set_lang.Unexpanded.t t -> bool

  val decode : Ordered_set_lang.Unexpanded.t t Dune_lang.Decoder.t

  val default : profile:Profile.t -> string list t

  val empty : Ordered_set_lang.Unexpanded.t t
end
