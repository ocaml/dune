open! Stdune

type stanza = Stanza.t = ..

module Stanza : sig
  type config =
    { flags          : Ocaml_flags.Spec.t
    ; c_flags        : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; env_vars       : Env.t
    ; binaries       : File_bindings.Unexpanded.t
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  val c_flags
    :  since:Syntax.Version.t option
    -> Ordered_set_lang.Unexpanded.t C.Kind.Dict.t Stanza.Decoder.fields_parser

  val decode : t Dune_lang.Decoder.t

  val find : t -> profile:string -> config option
end

type stanza +=
  | T of Stanza.t
