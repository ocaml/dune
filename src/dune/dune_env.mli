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

  type config =
    { flags : Ocaml_flags.Spec.t
    ; c_flags : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; env_vars : Env.t
    ; binaries : File_binding.Unexpanded.t list
    ; inline_tests : Inline_tests.t option
    }

  type pattern =
    | Profile of Profile.t
    | Any

  type t =
    { loc : Loc.t
    ; rules : (pattern * config) list
    }

  val c_flags :
       since:Dune_lang.Syntax.Version.t option
    -> Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
       Dune_lang.Decoder.fields_parser

  val decode : t Dune_lang.Decoder.t

  val empty : t

  val find : t -> profile:Profile.t -> config
end

type stanza += T of Stanza.t
