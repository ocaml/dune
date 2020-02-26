open! Stdune

type stanza = Stanza.t = ..

module Stanza : sig
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
    ; foreign_flags : Ordered_set_lang.Unexpanded.t Foreign.Language.Dict.t
    ; env_vars : Env.t
    ; binaries : File_binding.Unexpanded.t list
    ; inline_tests : Inline_tests.t option
    ; menhir_flags : Ordered_set_lang.Unexpanded.t
    ; odoc : Odoc.t
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

  val foreign_flags :
       since:Dune_lang.Syntax.Version.t option
    -> Ordered_set_lang.Unexpanded.t Foreign.Language.Dict.t
       Dune_lang.Decoder.fields_parser

  val decode : t Dune_lang.Decoder.t

  val empty : t

  val find : t -> profile:Profile.t -> config
end

type stanza += T of Stanza.t
