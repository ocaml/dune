open Stdune
open Build.O

type t =
  { build_dir : Path.t
  }

let make ~build_dir =
  { build_dir
  }

module V = Vfile_kind.Make(struct
    type t = string option
    let encode = Dune_lang.Encoder.(option string)
    let name = "Pkg_version"
  end)

let spec { build_dir } (p : Package.t) =
  let fn =
    Path.relative (Path.append build_dir p.path)
      (sprintf "%s.version.sexp" (Package.Name.to_string p.name))
  in
  Build.Vspec.T (fn, (module V))

let read t p = Build.vpath (spec t p)

let rule t p get = get >>> Build.store_vfile (spec t p)
