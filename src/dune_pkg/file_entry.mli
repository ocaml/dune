open Stdune

type source =
  | Path of Path.t
  | Content of string

type t =
  { original : source
  ; local_file : Path.Local.t
  }
