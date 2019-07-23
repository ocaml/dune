open Stdune

type t =
  { ctx_build_dir : Path.Build.t
  ; pkg : Package.t
  }

let to_dyn t = Package.to_dyn t.pkg

let hash t =
  Hashtbl.hash
    ( Package.hash t.pkg
    )

module Of_sctx = struct
  module Output = struct
    type nonrec t = t Package.Name.Map.t

    let equal = Package.Name.Map.equal ~equal:(==)

    let hash t =
      Package.Name.Map.to_list t
      |> List.hash (fun (k, v) ->
        Hashtbl.hash (Package.Name.hash k, hash v))

    let to_dyn t =
      Dyn.Map (
        Package.Name.Map.to_list t
        |> List.map ~f:(fun (k, v) -> (Package.Name.to_dyn k, to_dyn v)))
  end

  let def =
    let f sctx =
      let ctx = Super_context.context sctx in
      Super_context.packages sctx
      |> Package.Name.Map.map ~f:(fun (pkg : Package.t) ->
        { pkg
        ; ctx_build_dir = ctx.build_dir
        }
      )
    in
    Memo.create "of-sctx-def"
      ~doc:"mapping from package names to local packages"
      ~input:(module Super_context.As_memo_key)
      ~output:(Allow_cutoff (module Output))
      ~visibility:Hidden
      Sync
      f
end

let of_sctx sctx = Memo.exec Of_sctx.def sctx

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

let local_packages_by_dir_def =
  let module Output = struct
    type nonrec t = t list Path.Build.Map.t

    let equal = Path.Build.Map.equal ~equal:(List.equal (==))

    let to_dyn t =
      let open Dyn in
      Dyn.Map (
        Path.Build.Map.to_list t
        |> List.map ~f:(fun (k, v) ->
          let v = List (List.map ~f:to_dyn v) in
          (Path.Build.to_dyn k, v)))
  end
  in
  let f local_packages =
    Package.Name.Map.values local_packages
    |> List.map ~f:(fun pkg ->
      let build_dir = build_dir pkg in
      (build_dir, pkg))
    |> Path.Build.Map.of_list_multi
  in
  Memo.create "local-package-by-dir"
    ~doc:"Map from paths to local packages"
    ~input:(module Of_sctx.Output)
    ~output:(Allow_cutoff (module Output))
    ~visibility:Hidden
    Sync
    f

let local_packages_by_dir = Memo.exec local_packages_by_dir_def

let defined_in sctx ~dir =
  let local_packages = of_sctx sctx in
  let by_build_dir = local_packages_by_dir local_packages in
  Path.Build.Map.find by_build_dir dir
  |> Option.value ~default:[]
