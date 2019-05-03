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

module Version_source = struct
  type t =
    | Package
    | Project

  let to_dyn t =
    Dyn.Variant
      ((match t with
         | Package -> "Package"
         | Project -> "Project"),
       [])
end

type t =
  { name    : Name.t
  ; path    : Path.Source.t
  ; version : (string * Version_source.t) option
  }

let hash { name; path; version } =
  Hashtbl.hash
    ( Name.hash name
    , Path.Source.hash path
    , Hashtbl.hash version
    )

let to_dyn { name; path; version } =
  let open Dyn in
  Record
    [ "name", Name.to_dyn name
    ; "path", Path.Source.to_dyn path
    ; "version",
      Option (Option.map ~f:(fun (v, s) ->
        Tuple [String v; Version_source.to_dyn s]) version)
    ]

let pp fmt t = Dyn.pp fmt (to_dyn t)

let opam_file t = Path.Source.relative_exn t.path (Name.opam_fn t.name)

let meta_file t = Path.Source.relative_exn t.path (Name.meta_fn t.name)
