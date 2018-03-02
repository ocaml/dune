
module Name = struct
  type t = string

  let of_string x = x

  let opam_fn t = t ^ ".opam"

  module Map = Import.String_map
  module Set = Import.String_set

  let pp = Format.pp_print_string
end


type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

let opam_file t = Path.relative t.path (Name.opam_fn t.name)
