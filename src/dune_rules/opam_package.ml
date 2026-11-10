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
            let dir = Path.Source.relative_fname (Dune_file.dir dune_file) stanza.dir in
            let key =
              let digest =
                Digest.repr
                  Repr.(pair Path.Source.repr Package_version.repr)
                  (dir, stanza.version)
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
            let dir = Path.Build.append_source (Context_name.build_dir context) dir in
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
  let open Action_builder.O in
  let paths = Opam_build.Paths.map_path paths ~f:Path.build in
  let cookie = Opam_build.Paths.install_cookie paths in
  let* () = Action_builder.path cookie in
  let { Opam_build.Install_cookie.Gen.files; variables } =
    Opam_build.Install_cookie.load_exn cookie
  in
  let+ () = Section.Map.values files |> List.concat |> Action_builder.paths in
  let variables =
    Package_variable_name.Map.superpose
      (Package_variable_name.Map.of_list_exn variables)
      (Opam_build.variables
         ~name:(Package.name stanza.package)
         ~version:stanza.version
         ~dev:stanza.dev)
  in
  { Opam_build.Dependency.paths
  ; files
  ; variables
  ; exported_env = stanza.recipe.exported_env
  ; export_paths = paths
  }
;;
