type t =
  { max_size : int
  ; action : Import.Path.Build.t -> Action.t
  }

val output_file : string

val decode :
  unit -> (t option, Dune_lang.Decoder.fields) Dune_lang.Decoder.parser
