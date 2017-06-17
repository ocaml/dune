type t =
  { name                   : string
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

let opam_file t = Path.relative t.path (t.name ^ ".opam")
