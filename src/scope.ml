open! Stdune
open Import

type t =
  { project : Dune_project.t
  ; db      : Lib.DB.t
  ; root    : Path.Build.t
  }

let root t = t.root
let name t = Dune_project.name t.project
let project t = t.project
let libs t = t.db

module DB = struct
  type scope = t

  type t =
    { by_name : scope Dune_project.Name.Map.t
    ; by_dir : scope Path.Build.Map.t
    ; context : string
    }

  let find_by_dir t dir =
    let rec loop d =
      if Path.Build.is_root d then
        Exn.code_error "Scope.DB.find_by_dir got an invalid path"
          [ "dir"    , Path.Build.to_sexp dir
          ; "context", Sexp.Encoder.string t.context
          ];
      match Path.Build.Map.find t.by_dir d with
      | Some s -> s
      | None ->
        begin match Path.Build.parent d with
        | Some d -> loop d
        | None ->
          Exn.code_error "find_by_dir: invalid directory"
            [ "d", Path.Build.to_sexp d
            ; "dir", Path.Build.to_sexp dir
            ]
        end
    in
    loop dir

  let find_by_name t name =
    match Dune_project.Name.Map.find t.by_name name with
    | Some x -> x
    | None ->
      let dune_project_sexp p = Dyn.to_sexp (Dune_project.Name.to_dyn p) in
      Exn.code_error "Scope.DB.find_by_name"
        [ "name"   , dune_project_sexp name
        ; "context", Sexp.Encoder.string t.context
        ; "names",
          Sexp.Encoder.(list dune_project_sexp)
            (Dune_project.Name.Map.keys t.by_name)
        ]

  let resolve t public_libs name : Lib.DB.Resolve_result.t =
    match Lib_name.Map.find public_libs name with
    | None -> Not_found
    | Some project ->
      let scope = find_by_name (Fdecl.get t) (Dune_project.name project) in
      Redirect (Some scope.db, name)

  let find_implementations t public_libs virt =
    Lib_name.Map.fold public_libs ~init:Variant.Map.empty ~f:(fun project acc ->
      let scope = find_by_name (Fdecl.get t) (Dune_project.name project) in
      Lib.DB.find_implementations scope.db virt
      |> Variant.Map.Multi.rev_union acc)

  let public_libs t ~installed_libs internal_libs =
    let public_libs =
      List.filter_map internal_libs
        ~f:(fun (_dir, (lib : Dune_file.Library.t)) ->
          Option.map lib.public ~f:(fun p ->
            (Dune_file.Public_lib.name p, lib.project)))
      |> Lib_name.Map.of_list
      |> function
      | Ok x -> x
      | Error (name, _, _) ->
        match
          List.filter_map internal_libs ~f:(fun (_dir, lib) ->
            Option.bind lib.public ~f:(fun p ->
              Option.some_if (name = Dune_file.Public_lib.name p)
                lib.buildable.loc))
        with
        | [] | [_] -> assert false
        | loc1 :: loc2 :: _ ->
          die "Public library %a is defined twice:\n\
               - %s\n\
               - %s"
            Lib_name.pp_quoted name
            (Loc.to_file_colon_line loc1)
            (Loc.to_file_colon_line loc2)
    in
    let resolve = resolve t public_libs in
    let find_implementations = find_implementations t public_libs in
    Lib.DB.create ()
      ~parent:installed_libs
      ~resolve
      ~find_implementations
      ~all:(fun () -> Lib_name.Map.keys public_libs)

  let sccopes_by_name ~context ~projects ~lib_config ~public_libs
        internal_libs =
    let build_context_dir = Path.Build.relative Path.Build.root context in
    let projects_by_name =
      List.map projects ~f:(fun (project : Dune_project.t) ->
        (Dune_project.name project, project))
      |> Dune_project.Name.Map.of_list
      |> function
      | Ok x -> x
      | Error (_name, project1, project2) ->
        let to_sexp (project : Dune_project.t) =
          Dyn.Encoder.(pair Dune_project.Name.to_dyn Path.Source.to_dyn)
            (Dune_project.name project, Dune_project.root project)
          |> Dyn.to_sexp
        in
        Exn.code_error "Scope.DB.create got two projects with the same name"
          [ "project1", to_sexp project1
          ; "project2", to_sexp project2
          ]
    in
    let libs_by_project_name =
      List.map internal_libs ~f:(fun (dir, (lib : Dune_file.Library.t)) ->
        (Dune_project.name lib.project, (dir, lib)))
      |> Dune_project.Name.Map.of_list_multi
    in
    Dune_project.Name.Map.merge projects_by_name libs_by_project_name
      ~f:(fun _name project libs ->
        let project = Option.value_exn project in
        let libs = Option.value libs ~default:[] in
        let db = Lib.DB.create_from_library_stanzas libs ~parent:public_libs
                   ~lib_config in
        let root =
          Path.Build.append_source build_context_dir
            (Dune_project.root project) in
        Some { project; db; root })

  let create ~projects ~context ~installed_libs ~lib_config
        internal_libs =
    let t = Fdecl.create () in
    let public_libs = public_libs t ~installed_libs internal_libs in
    let by_name =
      sccopes_by_name ~context ~projects ~lib_config ~public_libs internal_libs
    in
    let by_dir =
      Dune_project.Name.Map.values by_name
      |> Path.Build.Map.of_list_map_exn ~f:(fun scope -> (scope.root, scope)) in
    Fdecl.set t { by_name ; by_dir ; context};
    (Fdecl.get t, public_libs)
end
