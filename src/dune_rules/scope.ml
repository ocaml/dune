open! Dune_engine
open! Stdune
open Import

type t =
  { project : Dune_project.t
  ; db : Lib.DB.t
  ; coq_db : Coq_lib.DB.t
  ; root : Path.Build.t
  }

let root t = t.root

let project t = t.project

let libs t = t.db

let coq_libs t = t.coq_db

module DB = struct
  type scope = t

  type t = { by_dir : scope Path.Source.Map.t }

  (* This function is linear in the depth of [dir] in the worst case, so if it
     shows up in the profile we should memoize it. *)
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
    | Name of (Loc.t * Lib_name.t)

  let resolve t public_libs name : Lib.DB.Resolve_result.t =
    match Lib_name.Map.find public_libs name with
    | None -> Lib.DB.Resolve_result.not_found
    | Some (Project project) ->
      let scope = find_by_project (Fdecl.get t) project in
      Lib.DB.Resolve_result.redirect (Some scope.db) (Loc.none, name)
    | Some (Name name) -> Lib.DB.Resolve_result.redirect None name

  (* Create a database from the public libraries defined in the stanzas *)
  let public_libs t ~installed_libs ~lib_config stanzas =
    let public_libs =
      List.filter_map stanzas
        ~f:(fun (stanza : Lib.DB.Library_related_stanza.t) ->
          match stanza with
          | Library (_, { project; public = Some p; _ }) ->
            Some (Dune_file.Public_lib.name p, Project project)
          | Library _
          | Library_redirect _ ->
            None
          | Deprecated_library_name s ->
            let old_name =
              Dune_file.Deprecated_library_name.old_public_name s
            in
            Some (old_name, Name s.new_public_name))
      |> Lib_name.Map.of_list
      |> function
      | Ok x -> x
      | Error (name, _, _) -> (
        match
          List.filter_map stanzas ~f:(fun stanza ->
              let named p loc = Option.some_if (name = p) loc in
              match stanza with
              | Library (_, { buildable = { loc; _ }; public = Some p; _ }) ->
                named (Dune_file.Public_lib.name p) loc
              | Deprecated_library_name d ->
                let old_name =
                  Dune_file.Deprecated_library_name.old_public_name d
                in
                named old_name d.loc
              | _ -> None)
        with
        | []
        | [ _ ] ->
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
    Lib.DB.create ~parent:(Some installed_libs) ~resolve
      ~all:(fun () -> Lib_name.Map.keys public_libs)
      ~lib_config ()

  let scopes_by_dir context ~projects ~public_libs stanzas coq_stanzas =
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
            | Library_redirect x -> x.project
            | Deprecated_library_name x -> x.project
          in
          (Dune_project.root project, stanza))
      |> Path.Source.Map.of_list_multi
    in
    let coq_stanzas_by_project_dir =
      List.map coq_stanzas ~f:(fun (dir, t) ->
          let project = t.Coq_stanza.Theory.project in
          (Dune_project.root project, (dir, t)))
      |> Path.Source.Map.of_list_multi
    in
    let stanzas_by_project_dir =
      Path.Source.Map.merge stanzas_by_project_dir coq_stanzas_by_project_dir
        ~f:(fun _dir stanzas coq_stanzas ->
          let stanza = Option.value stanzas ~default:[] in
          let coq_stanzas = Option.value coq_stanzas ~default:[] in
          Some (stanza, coq_stanzas))
    in
    let lib_config = Context.lib_config context in
    Path.Source.Map.merge projects_by_dir stanzas_by_project_dir
      ~f:(fun _dir project stanzas ->
        let project = Option.value_exn project in
        let stanzas, coq_stanzas = Option.value stanzas ~default:([], []) in
        let db =
          Lib.DB.create_from_stanzas stanzas ~parent:(Some public_libs)
            ~lib_config
        in
        let coq_db = Coq_lib.DB.create_from_coqlib_stanzas coq_stanzas in
        let root =
          Path.Build.append_source context.build_dir (Dune_project.root project)
        in
        Some { project; db; coq_db; root })

  let create ~projects ~context ~installed_libs stanzas coq_stanzas =
    let t = Fdecl.create Dyn.Encoder.opaque in
    let public_libs =
      let lib_config = Context.lib_config context in
      public_libs t ~installed_libs ~lib_config stanzas
    in
    let by_dir =
      scopes_by_dir context ~projects ~public_libs stanzas coq_stanzas
    in
    let value = { by_dir } in
    Fdecl.set t value;
    (value, public_libs)

  let find_by_dir t dir =
    if Path.Build.is_root dir then
      Code_error.raise "Scope.DB.find_by_dir got an invalid path"
        [ ("dir", Path.Build.to_dyn dir) ];
    find_by_dir t (Path.Build.drop_build_context_exn dir)

  let create_from_stanzas ~projects ~context ~installed_libs stanzas =
    let stanzas, coq_stanzas =
      Dune_load.Dune_file.fold_stanzas stanzas ~init:([], [])
        ~f:(fun dune_file stanza (acc, coq_acc) ->
          match stanza with
          | Dune_file.Library lib ->
            let ctx_dir =
              Path.Build.append_source context.Context.build_dir dune_file.dir
            in
            ( Lib.DB.Library_related_stanza.Library (ctx_dir, lib) :: acc
            , coq_acc )
          | Dune_file.Deprecated_library_name d ->
            (Deprecated_library_name d :: acc, coq_acc)
          | Dune_file.Library_redirect d -> (Library_redirect d :: acc, coq_acc)
          | Coq_stanza.Theory.T coq_lib ->
            let ctx_dir =
              Path.Build.append_source context.build_dir dune_file.dir
            in
            (acc, (ctx_dir, coq_lib) :: coq_acc)
          | _ -> (acc, coq_acc))
    in
    create ~projects ~context ~installed_libs stanzas coq_stanzas
end
