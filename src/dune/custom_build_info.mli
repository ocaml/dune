open! Stdune

type t =
  { max_size : int
  ; action : Loc.t * Action_dune_lang.t
  }

val output_file : string

val decode :
  unit -> (t option, Dune_lang.Decoder.fields) Dune_lang.Decoder.parser
