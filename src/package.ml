open! Stdune

module Name = struct
  module T = Interned.Make(struct
      let initial_size = 16
      let resize_policy = Interned.Conservative
      let order = Interned.Natural
    end)()

  include T

  let of_string = make

  let opam_fn (t : t) = to_string t ^ ".opam"

  let meta_fn (t : t) = "META." ^ to_string t

  let version_fn (t : t) = to_string t ^ ".version"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let decode = Dune_lang.Decoder.(map string ~f:of_string)

  let encode t = Dune_lang.Encoder.(string (to_string t))

  module Infix = Comparable.Operators(T)
end


type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

let pp fmt { name; path; version_from_opam_file } =
  Fmt.record fmt
    [ "name", Fmt.const Name.pp name
    ; "path", Fmt.const Path.pp path
    ; "version_from_opam_file", Fmt.const (Fmt.optional Format.pp_print_string) version_from_opam_file
    ]

let opam_file t = Path.relative t.path (Name.opam_fn t.name)

let meta_file t = Path.relative t.path (Name.meta_fn t.name)
