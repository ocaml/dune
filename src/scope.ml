open Import

type t =
  { project : Dune_project.t
  ; db      : Lib.DB.t
  ; root    : Path.t (* Path inside the build directory *)
  }

let root t = t.root
let name t = t.project.name
let project t = t.project
let libs t = t.db

module DB = struct
  type scope = t

  module Project_name_map = Map.Make(Dune_project.Name)

  type t =
    { by_dir  : (Path.t, scope) Hashtbl.t
    ; by_name : scope Project_name_map.t
    ; context : string
    }

  let find_by_dir t dir =
    let rec loop d =
      match Hashtbl.find t.by_dir d with
      | Some scope -> scope
      | None ->
        if Path.is_root d || not (Path.is_managed d) then
          Exn.code_error "Scope.DB.find_by_dir got an invalid path"
            [ "dir"    , Path.sexp_of_t dir
            ; "context", Sexp.To_sexp.string t.context
            ];
        let scope = loop (Path.parent_exn d) in
        Hashtbl.add t.by_dir d scope;
        scope
    in
    loop dir

  let find_by_name t name =
    match Project_name_map.find t.by_name name with
    | Some x -> x
    | None ->
      Exn.code_error "Scope.DB.find_by_name"
        [ "name"   , Dune_project.Name.sexp_of_t name
        ; "context", Sexp.To_sexp.string t.context
        ; "names",
          Sexp.To_sexp.(list Dune_project.Name.sexp_of_t)
            (Project_name_map.keys t.by_name)
        ]

  let create ~projects ~context ~installed_libs internal_libs =
    let projects_by_name =
      List.map projects ~f:(fun (project : Dune_project.t) ->
        (project.name, project))
      |> Project_name_map.of_list
      |> function
      | Ok x -> x
      | Error (_name, project1, project2) ->
        let to_sexp (project : Dune_project.t) =
          Sexp.To_sexp.(pair Dune_project.Name.sexp_of_t Path.Local.sexp_of_t)
            (project.name, project.root)
        in
        Exn.code_error "Scope.DB.create got two projects with the same name"
          [ "project1", to_sexp project1
          ; "project2", to_sexp project2
          ]
    in
    let libs_by_project_name =
      List.map internal_libs ~f:(fun (dir, (lib : Jbuild.Library.t)) ->
        (lib.project.name, (dir, lib)))
      |> Project_name_map.of_list_multi
    in
    let by_name_cell = ref Project_name_map.empty in
    let public_libs =
      let public_libs =
        List.filter_map internal_libs ~f:(fun (_dir, lib) ->
          match lib.public with
          | None -> None
          | Some p -> Some (Jbuild.Public_lib.name p, lib.project))
        |> String.Map.of_list
        |> function
        | Ok x -> x
        | Error (name, _, _) ->
          match
            List.filter_map internal_libs ~f:(fun (_dir, lib) ->
              match lib.public with
              | None   -> None
              | Some p -> Option.some_if (name = Jbuild.Public_lib.name p)
                            lib.buildable.loc)
          with
          | [] | [_] -> assert false
          | loc1 :: loc2 :: _ ->
            die "Public library %S is defined twice:\n\
                 - %s\n\
                 - %s"
              name
              (Loc.to_file_colon_line loc1)
              (Loc.to_file_colon_line loc2)
      in
      Lib.DB.create ()
        ~parent:installed_libs
        ~resolve:(fun name ->
          match String.Map.find public_libs name with
          | None -> Not_found
          | Some project ->
            let scope =
              Option.value_exn
                (Project_name_map.find !by_name_cell project.name)
            in
            Redirect (Some scope.db, name))
        ~all:(fun () -> String.Map.keys public_libs)
    in
    let by_name =
      let build_context_dir = Path.relative Path.build_dir context in
      Project_name_map.merge projects_by_name libs_by_project_name
        ~f:(fun _name project libs ->
          let project = Option.value_exn project in
          let libs = Option.value libs ~default:[] in
          let db =
            Lib.DB.create_from_library_stanzas libs ~parent:public_libs
          in
          let root = Path.append_local build_context_dir project.root in
          Some { project; db; root })
    in
    by_name_cell := by_name;
    let by_dir = Hashtbl.create 1024 in
    Project_name_map.iter by_name ~f:(fun scope ->
      Hashtbl.add by_dir scope.root scope);
    ({ by_name; by_dir; context }, public_libs)
end
