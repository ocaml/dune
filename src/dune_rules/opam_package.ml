open Import
open Memo.O

type t =
  { context : Context_name.t
  ; dir : Path.Build.t
  ; stanza : Opam_stanza.t
  ; paths : Path.Build.t Opam_build.Paths.t
  }

let key t =
  Path.Build.parent_exn t.paths.target_dir |> Path.Build.basename |> Filename.to_string
;;

let all =
  Per_context.create_by_name ~name:"opam-packages" (fun context ->
    Memo.lazy_ ~name:"opam-package-index" (fun () ->
      let+ dune_files = Dune_load.dune_files context in
      Dune_file.fold_static_stanzas
        dune_files
        ~init:Package.Name.Map.empty
        ~f:(fun dune_file stanza acc ->
          match Stanza.repr stanza with
          | Opam_stanza.T stanza ->
            let name = Package.name stanza.package in
            let source_dir =
              Source_path.relative_fname (Dune_file.source_dir dune_file) stanza.dir
            in
            let key =
              let digest =
                Digest.repr
                  Repr.(pair Source_path.repr Package_version.repr)
                  (source_dir, stanza.version)
                |> Digest.to_string
              in
              sprintf "%s-%s" (Package.Name.to_string name) digest
            in
            let root =
              Path.Build.L.relative
                Private_context.t.build_dir
                [ Context_name.to_string context; ".opam"; key ]
            in
            let paths =
              Opam_build.Paths.of_root name ~root ~relative:Path.Build.relative
            in
            let dir =
              Path.Build.relative_fname (Dune_file.output_dir dune_file) stanza.dir
            in
            if Package.Name.Map.mem acc name
            then
              User_error.raise
                ~loc:stanza.loc
                [ Pp.textf
                    "Package %s has more than one opam stanza"
                    (Package.Name.to_string name)
                ];
            Package.Name.Map.add_exn acc name { context; dir; stanza; paths }
          | _ -> acc))
    |> Memo.Lazy.force)
  |> Staged.unstage
;;

let find context name =
  let+ packages = all context in
  Package.Name.Map.find packages name
;;

let dependency { paths; stanza; _ } =
  Opam_build.Dependency.load
    (Opam_build.Paths.map_path paths ~f:Path.build)
    ~variables:
      (Opam_build.variables
         ~name:(Package.name stanza.package)
         ~version:stanza.version
         ~dev:stanza.dev)
    ~exported_env:stanza.recipe.exported_env
;;

let opaque_names context =
  let+ workspace = all context
  and+ mounted = Pkg_sources.mounted context in
  List.fold_left
    mounted
    ~init:(Package.Name.Set.of_keys workspace)
    ~f:(fun names mounted ->
      match Pkg_sources.Mounted.kind mounted with
      | Ok `Dune | Error _ -> names
      | Ok `Opam ->
        Package.Name.Set.add
          names
          (Pkg_sources.Mounted.candidate mounted |> Pkg_sources.Candidate.name))
;;

let find_opaque context name =
  let* workspace = find context name in
  match workspace with
  | Some package -> Memo.return (Some (package.paths.target_dir, dependency package))
  | None ->
    let* mounted = Pkg_sources.find_mounted context name in
    (match mounted with
     | None -> Memo.return None
     | Some mounted ->
       let* kind = Pkg_sources.Mounted.kind_exn mounted in
       (match kind with
        | `Dune -> Memo.return None
        | `Opam ->
          let candidate = Pkg_sources.Mounted.candidate mounted in
          let root = (Pkg_sources.Candidate.paths candidate).target_dir in
          let+ dependency = Pkg_rules.dependency context name in
          Option.map dependency ~f:(fun dependency -> root, dependency)))
;;
