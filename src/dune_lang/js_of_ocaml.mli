open Import

module Mode : sig
  type t =
    | JS
    | Wasm

  type mode := t

  val select : mode:t -> js:'a -> wasm:'a -> 'a
  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t
  val decode : t Decoder.t
  val to_dyn : t -> Dyn.t
  val all : t list

  module Pair : sig
    type 'a t =
      { js : 'a
      ; wasm : 'a
      }

    val select : mode:mode -> 'a t -> 'a
    val make : 'a -> 'a t
    val map : f:('a -> 'b) -> 'a t -> 'b t
    val mapi : f:(mode -> 'a -> 'b) -> 'a t -> 'b t
  end

  module Set : sig
    type t = bool Pair.t

    val inter : t -> t -> t
  end
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

  module Action_builder := Dune_engine.Action_builder

  val make
    :  spec:Spec.t
    -> default:string list Action_builder.t t
    -> eval:
         (Ordered_set_lang.Unexpanded.t
          -> standard:string list Action_builder.t
          -> string list Action_builder.t)
    -> string list Action_builder.t t

  val dump
    :  mode:Mode.t
    -> string list Action_builder.t t
    -> Dune_sexp.t list Action_builder.t
end

module Sourcemap : sig
  type t =
    | No
    | Inline
    | File
end

module Compilation_mode : sig
  type t =
    | Whole_program
    | Separate_compilation
end

module In_buildable : sig
  type t =
    { flags : Flags.Spec.t
    ; enabled_if : Blang.t option
    ; javascript_files : string list
    ; wasm_files : string list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  val decode : in_library:bool -> mode:Mode.t -> t Decoder.t
  val default : t
end

module In_context : sig
  type t =
    { flags : Flags.Spec.t
    ; enabled_if : Blang.t option
    ; javascript_files : Path.Build.t list
    ; wasm_files : Path.Build.t list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  val make : dir:Path.Build.t -> In_buildable.t Mode.Pair.t -> t Mode.Pair.t
  val default : t
  val force_whole_program_compilation : t Mode.Pair.t -> t Mode.Pair.t
end

module Ext : sig
  type t = string

  val exe : mode:Mode.t -> t
  val cmo : mode:Mode.t -> t
  val cma : mode:Mode.t -> t
  val runtime : mode:Mode.t -> t
  val wasm_dir : t
end

module Env : sig
  type 'a t =
    { compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    ; runtest_alias : Alias_name.t option
    ; flags : 'a Flags.t
    ; enabled_if : Blang.t option
    }

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val equal : Ordered_set_lang.Unexpanded.t t -> Ordered_set_lang.Unexpanded.t t -> bool
  val decode : mode:Mode.t -> Ordered_set_lang.Unexpanded.t t Decoder.t
  val default : profile:Profile.t -> string list t
  val empty : Ordered_set_lang.Unexpanded.t t
end
