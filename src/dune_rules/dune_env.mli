open Import

module Inline_tests : sig
  type t =
    | Enabled
    | Disabled
    | Ignored

  val decode : t Dune_lang.Decoder.t
  val to_string : t -> string
end

module Odoc : sig
  type warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings option }

  val decode : t Dune_lang.Decoder.t
end

type config =
  { flags : Ocaml_flags.Spec.t
  ; foreign_flags : Ordered_set_lang.Unexpanded.t Foreign_language.Dict.t
  ; link_flags : Link_flags.Spec.t
  ; env_vars : Env.t
  ; binaries : File_binding.Unexpanded.t list option
  ; inline_tests : Inline_tests.t option
  ; menhir : Ordered_set_lang.Unexpanded.t Menhir_env.t
  ; odoc : Odoc.t
  ; js_of_ocaml : Ordered_set_lang.Unexpanded.t Js_of_ocaml.Env.t
  ; coq : Coq_env.t
  ; format_config : Format_config.t option
  ; error_on_use : User_message.t option
  ; warn_on_load : User_message.t option
  ; bin_annot : bool option
  }

type pattern =
  | Profile of Profile.t
  | Any

type t =
  { loc : Loc.t
  ; rules : (pattern * config) list
  }

val hash : t -> int
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool

val foreign_flags
  :  since:Dune_lang.Syntax.Version.t option
  -> Ordered_set_lang.Unexpanded.t Foreign_language.Dict.t Dune_lang.Decoder.fields_parser

val decode : t Dune_lang.Decoder.t
val empty : t
val find_opt : t -> profile:Profile.t -> config option
val find : t -> profile:Profile.t -> config
val add_error : t -> message:User_message.t -> t
val add_warning : t -> message:User_message.t -> t
val fire_hooks : t -> profile:Profile.t -> unit

include Stanza.S with type t := t
