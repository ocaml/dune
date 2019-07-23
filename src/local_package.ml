open Stdune

type t =
  { ctx_build_dir : Path.Build.t
  ; pkg : Package.t
  }

let make ~(ctx : Context.t) ~pkg =
  { ctx_build_dir = ctx.build_dir
  ; pkg
  }

let to_dyn t = Package.to_dyn t.pkg

let hash t =
  Hashtbl.hash
    ( Package.hash t.pkg
    )

let package t = t.pkg
let opam_file t =
  Path.Build.append_source t.ctx_build_dir (Package.opam_file t.pkg)
let meta_file t =
  Path.Build.append_source t.ctx_build_dir (Package.meta_file t.pkg)
let build_dir t =
  Path.Build.append_source t.ctx_build_dir t.pkg.path

let name t = t.pkg.name
let dune_package_file t =
  Path.Build.relative (build_dir t)
    (Package.Name.to_string (name t) ^ ".dune-package")

let install_paths t =
  Install.Section.Paths.make ~package:t.pkg.name ~destdir:Path.root ()

let meta_template t =
  Path.Build.extend_basename (meta_file t) ~suffix:".template"
