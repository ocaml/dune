open Import

let opam_file (ctx : Context.t) (pkg : Package.t) =
  let opam_file = Package.opam_file pkg in
  let exists =
    match pkg.has_opam_file with
    | Exists b -> b
    | Generated -> true
  in
  if exists then Some (Path.Build.append_source ctx.build_dir opam_file)
  else None

let meta_file (ctx : Context.t) pkg =
  Path.Build.append_source ctx.build_dir (Package.meta_file pkg)

let deprecated_meta_file (ctx : Context.t) pkg name =
  Path.Build.append_source ctx.build_dir (Package.deprecated_meta_file pkg name)

let build_dir (ctx : Context.t) (pkg : Package.t) =
  let dir = Package.dir pkg in
  Path.Build.append_source ctx.build_dir dir

let dune_package_file ctx pkg =
  let name = Package.name pkg in
  Path.Build.relative (build_dir ctx pkg)
    (Package.Name.to_string name ^ ".dune-package")

let deprecated_dune_package_file ctx pkg name =
  Path.Build.relative (build_dir ctx pkg)
    (Package.Name.to_string name ^ ".dune-package")

let meta_template ctx pkg =
  Path.Build.extend_basename (meta_file ctx pkg) ~suffix:".template"
