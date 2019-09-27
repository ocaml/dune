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
      | Some s -> s
      | None -> (
        match Path.Source.parent d with
        | Some d -> loop d
        | None ->
          Code_error.raise "find_by_dir: invalid directory"
            [ ("d", Path.Source.to_dyn d); ("dir", Path.Source.to_dyn dir) ] )
    in
    loop dir

  let find_by_project t project =
    Path.Source.Map.find_exn t.by_dir (Dune_project.root project)

  type redirect_to =
    | Project of Dune_project.t
    | Name of Lib_name.t

  let resolve t public_libs name : Lib.DB.Resolve_result.t =
    match Lib_name.Map.find public_libs name with
    | None -> Not_found
    | Some (Project project) ->
      let scope = find_by_project (Fdecl.get t) project in
      Redirect (Some scope.db, name)
    | Some (Name name) -> Redirect (None, name)

  let public_libs t ~stdlib_dir ~installed_libs stanzas =
    let public_libs =
      List.filter_map stanzas
        ~f:(fun (stanza : Lib.DB.Library_related_stanza.t) ->
          match stanza with
          | Library (_, { project; public = Some p; _ }) ->
            Some (Dune_file.Public_lib.name p, Project project)
          | Library _ -> None
          | External_variant _ -> None
          | Deprecated_library_name x ->
            Some
              ( Dune_file.Public_lib.name x.old_public_name
              , Name x.new_public_name ))
      |> Lib_name.Map.of_list
      |> function
      | Ok x -> x
      | Error (name, _, _) -> (
        match
          List.filter_map stanzas ~f:(fun stanza ->
              match stanza with
              | Library (_, { buildable = { loc; _ }; public = Some p; _ })
               |Deprecated_library_name { loc; old_public_name = p; _ } ->
                Option.some_if (name = Dune_file.Public_lib.name p) loc
              | _ -> None)
        with
        | []
         |[ _ ] ->
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

  let scopes_by_dir context ~projects ~lib_config ~public_libs stanzas =
    let build_context_dir = Path.Build.relative Path.Build.root context in
    let projects_by_dir =
      List.map projects ~f:(fun (project : Dune_project.t) ->
          (Dune_project.root project, project))
      |> Path.Source.Map.of_list_exn
    in
    let stanzas_by_project_dir =
      List.map stanzas ~f:(fun (stanza : Lib.DB.Library_related_stanza.t) ->
          let project =
            match stanza with
            | Library (_, lib) -> lib.project
            | External_variant ev -> ev.project
            | Deprecated_library_name x -> x.project
          in
          (Dune_project.root project, stanza))
      |> Path.Source.Map.of_list_multi
    in
    Path.Source.Map.merge projects_by_dir stanzas_by_project_dir
      ~f:(fun _dir project stanzas ->
        let project = Option.value_exn project in
        let stanzas = Option.value stanzas ~default:[] in
        let db =
          Lib.DB.create_from_stanzas stanzas ~parent:public_libs ~lib_config
        in
        let root =
          Path.Build.append_source build_context_dir
            (Dune_project.root project)
        in
        Some { project; db; root })

  let create ~projects ~context ~installed_libs ~lib_config stanzas =
    let t = Fdecl.create Dyn.Encoder.opaque in
    let public_libs =
      public_libs t ~stdlib_dir:lib_config.Lib_config.stdlib_dir
        ~installed_libs stanzas
    in
    let by_dir =
      scopes_by_dir context ~projects ~lib_config ~public_libs stanzas
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
