open Stdune

let correct_specific
      ((opam : Dune_project.Opam.t)
      , { Dune_project.Opam.Package.synopsis
        ; description
        ; depends
        ; conflicts
        ; name = _
        }) =
  let open Opam_file.Mutator in
  set_string "synopsis" synopsis >>>
  set_string "description" description >>>
  list (depends @ opam.depends)
    (set_list "depends" Dune_project.Opam.Dependency.opam_depend) >>>
  list (conflicts @ opam.conflicts)
    (set_list "conflicts" Dune_project.Opam.Dependency.opam_depend)

let correct project package_name =
  let open Opam_file.Mutator in
  opt (
    let open Option.O in
    let* opam = Dune_project.opam project in
    let+ pkg = Dune_project.Opam.find opam package_name in
    (opam, pkg))
    correct_specific >>>
  opt (Dune_project.license project) (set_string "license") >>>
  list (Dune_project.authors project) (set_list "authors" mkstring) >>>
  opt (Dune_project.version project) (set_string "version") >>>
  opt (Option.map ~f:(Format.asprintf "%a" Dune_project.Source_kind.pp)
         (Dune_project.source project)) (set_string "dev-repo") >>>
  set_string "opam-version" "2.0" >>>
  fixup

let add_rules sctx ~dir ~project =
  let open Build.O in
  Local_package.defined_in sctx ~dir
  |> List.iter ~f:(fun pkg ->
    let opam_path = Local_package.opam_file pkg in
    let expected_path = Path.extend_basename opam_path ~suffix:".expected" in
    let expected_rule =
      Build.contents opam_path >>^ (fun contents ->
        let opamfile = Opam_file.of_string ~path:opam_path contents in
        let package_name = Local_package.name pkg in
        let corrected =
          Opam_file.Mutator.apply (correct project package_name) opamfile in
        OpamPrinter.Preserved.items contents opamfile.file_contents
          corrected.file_contents) >>>
      Build.write_file_dyn expected_path
    in
    let diff_rule =
      Build.paths [expected_path; opam_path]
      >>^ fun () ->
      Action.Diff { Action.Diff.
                    file1 = opam_path
                  ; file2 = expected_path
                  ; optional = false
                  ; mode = Text
                  }
    in
    let dir = Local_package.build_dir pkg in
    Super_context.add_rule sctx ~dir expected_rule;
    let aliases =
      [ Alias.install ~dir
      ; Alias.runtest ~dir ] in
    List.iter ~f:(fun alias ->
      Super_context.add_alias_action sctx alias
        ~dir ~loc:None ~stamp:("opam_diff", opam_path) diff_rule)
      aliases)

let add_rules sctx ~dir =
  let scope = Super_context.find_scope_by_dir sctx dir in
  let project = Scope.project scope in
  Option.iter
    (Dune_project.opam project)
    ~f:(fun _ -> add_rules sctx ~dir ~project)
