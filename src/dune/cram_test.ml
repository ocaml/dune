open! Stdune

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

let is_cram_suffix = String.is_suffix ~suffix:".t"
