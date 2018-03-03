
module Name = struct
  include Interned.Make()

  let of_string = make

  let opam_fn (t : t) = to_string t ^ ".opam"

  let pp fmt t = Format.pp_print_string fmt (to_string t)
end


type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

let opam_file t = Path.relative t.path (Name.opam_fn t.name)
