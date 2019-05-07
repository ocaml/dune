open Stdune

let default_build_command = lazy (
  let path = Path.of_string "<internal>" in
  let dummy_opam =
      Opam_file.of_string ~path {|
build: [
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs]
  [ "dune" "runtest" "-p" name "-j" jobs] {with-test}
  [ "dune" "build" "-p" name "@doc"] {with-doc}
]
|}
  in
  Opam_file.get_field dummy_opam "build"
  |> Option.value_exn)

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
      ; tags = _
      ; loc = _
      } =
  let open Opam_file.Create in
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
  optional @ dep_fields

let opam_fields project (package : Package.t) =
  let package_fields = package_fields package in
  let open Opam_file.Create in
  let optional_fields =
    [ "bug-reports", Dune_project.bug_reports project
    ; "homepage", Dune_project.homepage project
    ; "license", Dune_project.license project
    ; "version", Dune_project.version project
    ; "dev-repo",
      Option.map ~f:(Format.asprintf "%a" (Dune_project.Source_kind.pp))
        (Dune_project.source project)
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | None -> None
      | Some v -> Some (k, string v))
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
    ; "build", (Lazy.force default_build_command)
    ]
  in
  List.concat
    [ fields
    ; list_fields
    ; optional_fields
    ; package_fields
    ]

let opam_template sctx ~pkg =
  let file_tree = Super_context.file_tree sctx in
  let opam_template_path =
    Package.opam_file pkg
    |> Path.Source.extend_basename ~suffix:".template"
  in
  if File_tree.file_exists file_tree opam_template_path then
    let build_dir = Super_context.build_dir sctx in
    Some (Path.append_source build_dir opam_template_path)
  else
    None

let add_rule sctx ~project ~pkg =
  let open Build.O in
  let opam_path = Path.build (Local_package.opam_file pkg) in
  let opam_rule =
    (match opam_template sctx ~pkg:(Local_package.package pkg) with
     | Some p -> Build.contents p
     | None -> Build.return "")
    >>>
    Build.arr (fun template ->
      let opamfile = Opam_file.of_string ~path:opam_path template in
      let existing_vars_template = Opam_file.existing_variables opamfile in
      let generated_fields =
        let package = Local_package.package pkg in
        opam_fields project package
        |> List.filter ~f:(fun (v, _) ->
          not (String.Set.mem existing_vars_template v))
        |> Opam_file.Create.of_bindings ~file:opam_path
      in
      sprintf "%s\n%s"
        (OpamPrinter.opamfile generated_fields)
        template)
    >>> Build.write_file_dyn opam_path
  in
  let dir = Path.build (Local_package.build_dir pkg) in
  let mode =
    Dune_file.Rule.Mode.Promote
      { lifetime = Unlimited
      ; into = None
      ; only = None
      }
  in
  Super_context.add_rule sctx ~mode ~dir opam_rule;
  let aliases =
    [ Alias.install ~dir
    ; Alias.runtest ~dir
    ; Alias.check ~dir (* check doesn't pick up the promote target? *)
    ]
  in
  let deps = Path.Set.singleton opam_path in
  List.iter aliases ~f:(fun alias ->
    Build_system.Alias.add_deps alias deps)

let add_rules sctx ~dir =
  let project =
    let scope = Super_context.find_scope_by_dir sctx dir in
    Scope.project scope
  in
  if Dune_project.generate_opam_files project then begin
    Local_package.defined_in sctx ~dir
    |> List.iter ~f:(fun pkg ->
      let package = Local_package.package pkg in
      match package.kind with
      | Dune _ -> add_rule sctx ~project ~pkg
      | Opam -> ())
  end
