open! Stdune
open Import

type t =
  { project : Dune_project.t
  ; db : Lib.DB.t
  ; root : Path.Build.t
  }

let root t = t.root

let name t = Dune_project.name t.project

let project t = t.project

let libs t = t.db

module DB = struct
  type scope = t

  type t =
    { by_dir : scope Path.Source.Map.t
    ; context : string
    }

  let find_by_dir t (dir : Path.Source.t) =
    let rec loop d =
      match Path.Source.Map.find t.by_dir d with
      | Some s ->
        s
      | None -> (
        match Path.Source.parent d with
        | Some d ->
          loop d
        | None ->
          Code_error.raise "find_by_dir: invalid directory"
            [ ("d", Path.Source.to_dyn d); ("dir", Path.Source.to_dyn dir) ] )
    in
    loop dir

  let find_by_project t project =
    Path.Source.Map.find_exn t.by_dir (Dune_project.root project)

  let resolve t public_libs name : Lib.DB.Resolve_result.t =
    match Lib_name.Map.find public_libs name with
    | None ->
      Not_found
    | Some project ->
      let scope = find_by_project (Fdecl.get t) project in
      Redirect (Some scope.db, name)

  let public_libs t ~stdlib_dir ~installed_libs internal_libs =
    let public_libs =
      List.filter_map internal_libs
        ~f:(fun (_dir, (lib : Dune_file.Library.t)) ->
          Option.map lib.public ~f:(fun p ->
              (Dune_file.Public_lib.name p, lib.project)))
      |> Lib_name.Map.of_list
      |> function
      | Ok x ->
        x
      | Error (name, _, _) -> (
        match
          List.filter_map internal_libs ~f:(fun (_dir, lib) ->
              Option.bind lib.public ~f:(fun p ->
                  Option.some_if
                    (name = Dune_file.Public_lib.name p)
                    lib.buildable.loc))
        with
        | [] | [ _ ] ->
          assert false
        | loc1 :: loc2 :: _ ->
          User_error.raise
            [ Pp.textf "Public library %s is defined twice:"
                (Lib_name.to_string name)
            ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
            ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
            ] )
    in
    let resolve = resolve t public_libs in
    Lib.DB.create () ~stdlib_dir ~parent:installed_libs ~resolve
      ~all:(fun () -> Lib_name.Map.keys public_libs)

  let scopes_by_dir context ~projects ~lib_config ~public_libs internal_libs
      variant_implementations =
    let build_context_dir = Path.Build.relative Path.Build.root context in
    let projects_by_dir =
      List.map projects ~f:(fun (project : Dune_project.t) ->
          (Dune_project.root project, project))
      |> Path.Source.Map.of_list_exn
    in
    let libs_by_project_dir =
      List.map internal_libs ~f:(fun (dir, (lib : Dune_file.Library.t)) ->
          (Dune_project.root lib.project, (dir, lib)))
      |> Path.Source.Map.of_list_multi
    in
    let variant_implementations_by_project_dir =
      List.map variant_implementations
        ~f:(fun (lib : Dune_file.External_variant.t) ->
          (Dune_project.root lib.project, lib))
      |> Path.Source.Map.of_list_multi
    in
    let libs_variants_by_project_dir =
      Path.Source.Map.merge libs_by_project_dir
        variant_implementations_by_project_dir ~f:(fun _dir libs variants ->
          let libs = Option.value libs ~default:[] in
          let variants = Option.value variants ~default:[] in
          Some (libs, variants))
    in
    Path.Source.Map.merge projects_by_dir libs_variants_by_project_dir
      ~f:(fun _dir project l_v ->
        let project = Option.value_exn project in
        let libs, variants = Option.value l_v ~default:([], []) in
        let db =
          Lib.DB.create_from_library_stanzas libs variants ~parent:public_libs
            ~lib_config
        in
        let root =
          Path.Build.append_source build_context_dir
            (Dune_project.root project)
        in
        Some { project; db; root })

  let create ~projects ~context ~installed_libs ~lib_config internal_libs
      variant_implementations =
    let t = Fdecl.create () in
    let public_libs =
      public_libs t ~stdlib_dir:lib_config.Lib_config.stdlib_dir
        ~installed_libs internal_libs
    in
    let by_dir =
      scopes_by_dir context ~projects ~lib_config ~public_libs internal_libs
        variant_implementations
    in
    let value = { by_dir; context } in
    Fdecl.set t value;
    (value, public_libs)

  let find_by_dir t dir =
    if Path.Build.is_root dir then
      Code_error.raise "Scope.DB.find_by_dir got an invalid path"
        [ ("dir", Path.Build.to_dyn dir)
        ; ("context", Dyn.Encoder.string t.context)
        ];
    find_by_dir t (Path.Build.drop_build_context_exn dir)
end
