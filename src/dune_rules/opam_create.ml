open Import
open Memo.O
module Opam_file = Dune_pkg.Opam_file

let default_build_command =
  let before_1_11 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string
            ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs]
  [ "dune" "runtest" "-p" name "-j" jobs] {with-test}
  [ "dune" "build" "-p" name "@doc"] {with-doc}
]
|}))
  and from_1_11_before_2_7 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string
            ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
]
|}))
  and from_2_7 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string
            ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {dev}
  [ "dune" "build" "-p" name "-j" jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
]
|}))
  and from_2_9 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string
            ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {dev}
  [ "dune" "build" "-p" name "-j" jobs "--promote-install-files=false"
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
  [ "dune" "install" "-p" name "--create-install-files" name ]
]
|}))
  and from_3_0 ~with_subst ~with_sites =
    let subst = if with_subst then {|  [ "dune" "subst" ] {dev} |} else "" in
    let promote_install_files =
      if with_sites then {|  "--promote-install-files=false" |} else ""
    in
    let install =
      if with_sites
      then {| [ "dune" "install" "-p" name "--create-install-files" name ] |}
      else ""
    in
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string
            ~fname:"<internal>"
            (Printf.sprintf
               {|
[
  %s
  [ "dune" "build" "-p" name "-j" jobs %s
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
  %s
]
|}
               subst
               promote_install_files
               install)))
  in
  fun project ->
    Lazy.force
      (if Dune_project.dune_version project < (1, 11)
       then before_1_11
       else if Dune_project.dune_version project < (2, 7)
       then from_1_11_before_2_7
       else if Dune_project.dune_version project < (2, 9)
       then from_2_7
       else if Dune_project.dune_version project < (3, 0)
       then from_2_9
       else
         from_3_0
           ~with_subst:(Toggle.enabled (snd (Dune_project.subst_config project)))
           ~with_sites:Dune_project.(is_extension_set project dune_site_extension))
;;

let package_fields package ~project =
  let open Opam_file.Create in
  let tags =
    let tags = Package.tags package in
    if tags = [] then [] else [ "tags", string_list tags ]
  in
  let optional =
    [ "synopsis", Package.synopsis package; "description", Package.description package ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | None -> None
      | Some v -> Some (k, string v))
  in
  let dep_fields =
    [ "depends", Package.depends package
    ; "conflicts", Package.conflicts package
    ; "depopts", Package.depopts package
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _ :: _ -> Some (k, list Dune_pkg.Package_dependency.opam_depend v))
  in
  let fields = [ optional; dep_fields ] in
  let fields =
    let dune_version = Dune_project.dune_version project in
    if dune_version >= (2, 0) && tags <> [] then tags :: fields else fields
  in
  List.concat fields
;;

let dune_name = Package.Name.of_string "dune"
let odoc_name = Package.Name.of_string "odoc"

let insert_dune_dep depends dune_version =
  let constraint_ : Package_constraint.t =
    let dune_version = Dune_lang.Syntax.Version.to_string dune_version in
    Uop (Gte, String_literal dune_version)
  in
  let rec loop acc = function
    | [] ->
      let dune_dep =
        { Package_dependency.name = dune_name; constraint_ = Some constraint_ }
      in
      dune_dep :: List.rev acc
    | (dep : Package_dependency.t) :: rest ->
      if Package.Name.equal dep.name dune_name
      then (
        let dep =
          if dune_version < (2, 6)
          then dep
          else
            { dep with
              constraint_ =
                Some
                  (match dep.constraint_ with
                   | None -> constraint_
                   | Some c -> And [ constraint_; c ])
            }
        in
        List.rev_append acc (dep :: rest))
      else loop (dep :: acc) rest
  in
  loop [] depends
;;

let rec already_requires_odoc : Package_constraint.t -> bool = function
  | Uop _ | Bop _ -> true
  | Bvar var -> Dune_lang.Package_variable_name.(one_of var [ with_doc; build; post ])
  | And l -> List.for_all ~f:already_requires_odoc l
  | Or l -> List.exists ~f:already_requires_odoc l
;;

let insert_odoc_dep depends =
  let with_doc : Package_constraint.t = Bvar Dune_lang.Package_variable_name.with_doc in
  let odoc_dep = { Package_dependency.name = odoc_name; constraint_ = Some with_doc } in
  let rec loop acc = function
    | [] -> List.rev (odoc_dep :: acc)
    | (dep : Package_dependency.t) :: rest ->
      if Package.Name.equal dep.name odoc_name
         && Option.forall ~f:already_requires_odoc dep.constraint_
      then (* Stop now as odoc will be required anyway *)
        List.rev_append (dep :: acc) rest
      else loop (dep :: acc) rest
  in
  loop [] depends
;;

let opam_fields project (package : Package.t) =
  let dune_version = Dune_project.dune_version project in
  let package_name = Package.name package in
  let package =
    if dune_version < (1, 11) || Package.Name.equal package_name dune_name
    then package
    else
      Package.map_depends package ~f:(fun depends -> insert_dune_dep depends dune_version)
  in
  let package =
    if dune_version < (2, 7) || Package.Name.equal package_name odoc_name
    then package
    else Package.map_depends package ~f:insert_odoc_dep
  in
  let package_fields = package_fields package ~project in
  let open Opam_file.Create in
  let info = Package.info package in
  let optional_fields =
    [ "bug-reports", Package_info.bug_reports info
    ; "homepage", Package_info.homepage info
    ; "doc", Package_info.documentation info
    ; ( "license"
      , match Package_info.license info with
        | Some [ x ] -> Some x
        | _ -> None )
    ; "version", Option.map ~f:Package_version.to_string (Package.version package)
    ; "dev-repo", Option.map ~f:Source_kind.to_string (Package_info.source info)
    ]
    |> List.filter_map ~f:(fun (k, v) -> Option.map v ~f:(fun v -> k, string v))
  in
  let list_fields =
    [ "maintainer", Package_info.maintainers info
    ; "authors", Package_info.authors info
    ; ( "license"
      , match Package_info.license info with
        | None | Some [ _ ] -> None
        | Some l -> Some l )
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | None | Some [] -> None
      | Some (_ :: _ as v) -> Some (k, string_list v))
  in
  let fields = [ "opam-version", string "2.0"; "build", default_build_command project ] in
  let fields = List.concat [ fields; list_fields; optional_fields; package_fields ] in
  if dune_version < (1, 11) then fields else Opam_file.Create.normalise_field_order fields
;;

let template_file = Path.extend_basename ~suffix:".template"

let opam_template ~opam_path =
  let open Action_builder.O in
  let opam_template_path = template_file opam_path in
  Action_builder.if_file_exists
    opam_template_path
    ~then_:
      (let+ contents = Action_builder.contents opam_template_path in
       Some (opam_template_path, contents))
    ~else_:(Action_builder.return None)
;;

let generate project pkg ~template =
  let opam_fname = Package.opam_file pkg in
  let filter_fields =
    match template with
    | None -> Fun.id
    | Some (fname, contents) ->
      let vars_in_template =
        Lexbuf.from_string ~fname:(Path.to_string fname) contents
        |> Opam_file.parse
        |> Opam_file.existing_variables
      in
      List.filter ~f:(fun (v, _) -> not (String.Set.mem vars_in_template v))
  in
  let generated_fields =
    opam_fields project pkg
    |> filter_fields
    |> Opam_file.Create.of_bindings ~file:(Path.source opam_fname)
  in
  sprintf
    "# This file is generated by dune, edit dune-project instead\n%s\n%s"
    (OpamPrinter.FullPos.opamfile generated_fields)
    (match template with
     | None -> ""
     | Some (_, s) -> s)
;;

let add_alias_rule (ctx : Build_context.t) ~project ~pkg =
  let build_dir = ctx.build_dir in
  let dir = Path.Build.append_source build_dir (Dune_project.root project) in
  let opam_path = Path.Build.append_source build_dir (Package.opam_file pkg) in
  let aliases =
    [ Alias.make Alias0.install ~dir
    ; Alias.make Alias0.runtest ~dir
    ; Alias.make Alias0.check ~dir (* check doesn't pick up the promote target? *)
    ]
  in
  let deps = Path.Set.singleton (Path.build opam_path) in
  Memo.parallel_iter aliases ~f:(fun alias ->
    (* TODO slow. we should be calling these functions only once, rather than
       once per package *)
    Rules.Produce.Alias.add_deps alias (Action_builder.path_set deps))
;;

let add_opam_file_rule sctx ~project ~pkg =
  let open Action_builder.O in
  let build_dir = Super_context.context sctx |> Context.build_dir in
  let opam_path = Path.Build.append_source build_dir (Package.opam_file pkg) in
  let opam_rule =
    (let+ template = opam_template ~opam_path:(Path.build opam_path) in
     generate project pkg ~template)
    |> Action_builder.write_file_dyn opam_path
  in
  let dir = Path.Build.append_source build_dir (Dune_project.root project) in
  let mode = Rule.Mode.Promote { lifetime = Unlimited; into = None; only = None } in
  Super_context.add_rule sctx ~mode ~dir opam_rule
;;

let add_opam_file_rules sctx project =
  Memo.when_ (Dune_project.generate_opam_files project) (fun () ->
    let packages = Dune_project.packages project in
    Memo.parallel_iter_seq
      (Dune_lang.Package_name.Map.to_seq packages)
      ~f:(fun (_name, (pkg : Package.t)) -> add_opam_file_rule sctx ~project ~pkg))
;;

let add_rules sctx project =
  Memo.when_ (Dune_project.generate_opam_files project) (fun () ->
    let packages = Dune_project.packages project in
    Memo.parallel_iter_seq
      (Dune_lang.Package_name.Map.to_seq packages)
      ~f:(fun (_name, (pkg : Package.t)) ->
        let* () =
          add_alias_rule
            (Context.build_context (Super_context.context sctx))
            ~project
            ~pkg
        in
        match Dune_project.opam_file_location project with
        | `Inside_opam_directory -> Memo.return ()
        | `Relative_to_project -> add_opam_file_rule sctx ~project ~pkg))
;;

module Gen_rules = Build_config.Gen_rules

let gen_rules sctx ~dir ~nearest_src_dir ~src_dir =
  match nearest_src_dir with
  | None -> None
  | Some nearest_src_dir ->
    let project = Source_tree.Dir.project nearest_src_dir in
    let project_root = Dune_project.root project in
    (match Path.Source.is_descendant src_dir ~of_:project_root with
     | false -> None
     | true ->
       let project_root = Dune_project.root project in
       let project_rules = Path.Source.equal project_root src_dir in
       let opam_file_location = Dune_project.opam_file_location project in
       let opam_dir = "opam" in
       let inside_generated_opam_directory =
         match opam_file_location with
         | `Inside_opam_directory ->
           Path.Source.equal src_dir (Path.Source.relative project_root opam_dir)
         | `Relative_to_project -> false
       in
       if (not inside_generated_opam_directory) && not project_rules
       then None
       else (
         let allowed_subdirs =
           match opam_file_location with
           | `Inside_opam_directory when project_rules -> Filename.Set.singleton opam_dir
           | `Relative_to_project | `Inside_opam_directory -> Filename.Set.empty
         in
         let rules =
           Rules.collect_unit (fun () ->
             let* sctx = sctx in
             let+ () = if project_rules then add_rules sctx project else Memo.return ()
             and+ () =
               if inside_generated_opam_directory
               then add_opam_file_rules sctx project
               else Memo.return ()
             in
             ())
         in
         Some (Gen_rules.rules_for ~allowed_subdirs ~dir rules)))
;;
