open! Stdune

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

val is_cram_suffix : string -> bool
