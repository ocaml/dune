open Stdune

let default_build_command =
  let before_1_11 = lazy (
    Opam_file.parse_value
      (Lexbuf.from_string ~fname:"<internal>" {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs]
  [ "dune" "runtest" "-p" name "-j" jobs] {with-test}
  [ "dune" "build" "-p" name "@doc"] {with-doc}
]
|}))
  and from_1_11 = lazy (
    Opam_file.parse_value
      (Lexbuf.from_string ~fname:"<internal>" {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
]
|}))
  in
  fun project ->
    Lazy.force (
      if Dune_project.dune_version project < (1, 11) then
        before_1_11
      else
        from_1_11
    )

let package_fields
      { Package.synopsis
      ; description
      ; depends
      ; conflicts
      ; depopts
      ; name = _
      ; path = _
      ; version = _
      ; kind = _
      ; tags
      ; loc = _
      } ~project =
  let open Opam_file.Create in
  let tags = if tags = [] then [] else ["tags", string_list tags] in
  let optional =
    [ "synopsis", synopsis
    ; "description", description
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | None -> None
      | Some v -> Some (k, string v))
  in
  let dep_fields =
    [ "depends", depends
    ; "conflicts", conflicts
    ; "depopts", depopts
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _ :: _ -> Some (k, list Package.Dependency.opam_depend v))
  in
  let fields = [ optional; dep_fields ] in
  let fields =
    let dune_version = Dune_project.dune_version project in
    if dune_version >= (2, 0) && tags <> [] then
      tags :: fields
    else
      fields
  in
  List.concat fields

let opam_fields project (package : Package.t) =
  let dune_version = Dune_project.dune_version project in
  let dune_name = Package.Name.of_string "dune" in
  let package =
    if dune_version < (1, 11) || Package.Name.equal package.name dune_name then
      package
    else
      let dune_dep =
        let dune_version = Syntax.Version.to_string dune_version in
        let constraint_ : Package.Dependency.Constraint.t =
          Uop (Gte, QVar dune_version) in
        { Package.Dependency.
          name = dune_name
        ; constraint_ = Some constraint_
        }
      in
      let is_dune_depend (pkg : Package.Dependency.t) =
        Package.Name.equal pkg.name dune_dep.name in
      if List.exists package.depends ~f:is_dune_depend then
        package
      else
        { package with depends = dune_dep :: package.depends }
  in
  let package_fields = package_fields package ~project in
  let open Opam_file.Create in
  let optional_fields =
    [ "bug-reports", Dune_project.bug_reports project
    ; "homepage", Dune_project.homepage project
    ; "doc", Dune_project.documentation project
    ; "license", Dune_project.license project
    ; "version", Dune_project.version project
    ; "dev-repo",
      Option.map ~f:(Format.asprintf "%a" (Dune_project.Source_kind.pp))
        (Dune_project.source project)
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      Option.map v ~f:(fun v -> (k, string v)))
  in
  let list_fields =
    [ "maintainer", Dune_project.maintainers project
    ; "authors", Dune_project.authors project
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _::_ -> Some (k, string_list v))
  in
  let fields =
    [ "opam-version", string "2.0"
    ; "build", default_build_command project
    ]
  in
  let fields =
    List.concat
      [ fields
      ; list_fields
      ; optional_fields
      ; package_fields
      ]
  in
  if Dune_project.dune_version project < (1, 11) then
    fields
  else
    Opam_file.Create.normalise_field_order fields

let opam_template sctx ~pkg =
  let file_tree = Super_context.file_tree sctx in
  let opam_template_path =
    Package.opam_file pkg
    |> Path.Source.extend_basename ~suffix:".template"
  in
  if File_tree.file_exists file_tree opam_template_path then
    let build_dir = Super_context.build_dir sctx in
    Some (Path.Build.append_source build_dir opam_template_path)
  else
    None

let add_rule sctx ~project ~pkg =
  let open Build.O in
  let build_dir = Super_context.build_dir sctx in
  let opam_path = Path.Build.append_source build_dir (Package.opam_file pkg) in
  let opam_rule =
    (match opam_template sctx ~pkg with
     | Some p -> Build.contents (Path.build p)
     | None -> Build.return "")
    >>>
    Build.arr (fun template ->
      let opam_path = Path.build opam_path in
      let opamfile =
        Opam_file.parse (Lexbuf.from_string ~fname:(Path.to_string opam_path)
                           template)
      in
      let existing_vars_template = Opam_file.existing_variables opamfile in
      let generated_fields =
        opam_fields project pkg
        |> List.filter ~f:(fun (v, _) ->
          not (String.Set.mem existing_vars_template v))
        |> Opam_file.Create.of_bindings ~file:opam_path
      in
      sprintf
        "# This file is generated by dune, edit dune-project instead\n\
         %s\n%s"
        (OpamPrinter.opamfile generated_fields)
        template)
    >>> Build.write_file_dyn opam_path
  in
  let dir = Path.Build.append_source build_dir pkg.path in
  let mode =
    Dune_file.Rule.Mode.Promote
      { lifetime = Unlimited
      ; into = None
      ; only = None
      }
  in
  Super_context.add_rule
    sctx ~mode ~dir opam_rule;
  let aliases =
    [ Alias.install ~dir
    ; Alias.runtest ~dir
    ; Alias.check ~dir (* check doesn't pick up the promote target? *)
    ]
  in
  let deps = Path.Set.singleton (Path.build opam_path) in
  List.iter aliases ~f:(fun alias ->
    Rules.Produce.Alias.add_deps alias deps)

let add_rules sctx ~dir =
  let project =
    Super_context.find_scope_by_dir sctx dir
    |> Scope.project
  in
  if Dune_project.generate_opam_files project then begin
    Dune_project.packages project
    |> Package.Name.Map.iter ~f:(fun (pkg : Package.t) ->
      match pkg.kind with
      | Dune _ -> add_rule sctx ~project ~pkg
      | Opam -> ())
  end
