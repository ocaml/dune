open! Import
include Dune_lang.Package
module Opam_file = Dune_pkg.Opam_file

let load_opam_file_with_contents ~contents:opam_file_string file name =
  let loc = Loc.in_file (Path.source file) in
  let opam =
    let opam =
      let lexbuf =
        Lexbuf.from_string opam_file_string ~fname:(Path.Source.to_string file)
      in
      try Ok (Opam_file.parse lexbuf) with
      | User_error.E _ as exn -> Error exn
    in
    match opam with
    | Ok s -> Some s
    | Error exn ->
      (* CR-rgrinberg: make it possible to disable this warning *)
      User_warning.emit
        ~loc
        [ Pp.text
            "Unable to read opam file. Some information about this package such as its \
             version will be ignored."
        ; Pp.textf "Reason: %s" (Printexc.to_string exn)
        ];
      None
  in
  let open Option.O in
  let get_one name =
    let* value =
      let* opam = opam in
      Opam_file.get_field opam name
    in
    match value.pelem with
    | String s -> Some s
    | _ -> None
  in
  let get_many name =
    let* value =
      let* opam = opam in
      Opam_file.get_field opam name
    in
    match value.pelem with
    | String s -> Some [ s ]
    | List l ->
      List.fold_left
        l.pelem
        ~init:(Some [])
        ~f:(fun acc (v : OpamParserTypes.FullPos.value) ->
          let* acc = acc in
          match v.pelem with
          | String s -> Some (s :: acc)
          | _ -> None)
      >>| List.rev
    | _ -> None
  in
  let dir = Path.Source.parent_exn file in
  let info =
    Package_info.create
      ~maintainers:(get_many "maintainer")
      ~authors:(get_many "authors")
      ~homepage:(get_one "homepage")
      ~bug_reports:(get_one "bug-reports")
      ~documentation:(get_one "doc")
      ~license:(get_many "license")
      ~source:
        (let+ url = get_one "dev-repo" in
         Source_kind.Url url)
  in
  create
    ~name
    ~dir
    ~loc
    ~version:(get_one "version" |> Option.map ~f:Package_version.of_string)
    ~conflicts:[]
    ~depends:[]
    ~depopts:[]
    ~info
    ~synopsis:(get_one "synopsis")
    ~description:(get_one "description")
    ~has_opam_file:(Exists true)
    ~tags:(Option.value (get_many "tags") ~default:[])
    ~deprecated_package_names:Name.Map.empty
    ~sites:Site.Map.empty
    ~allow_empty:true
    ~original_opam_file:(Some { file; contents = opam_file_string })
;;

let to_local_package t =
  let loc = loc t in
  let version = version t in
  match original_opam_file t with
  | None ->
    { Dune_pkg.Local_package.name = name t
    ; version
    ; dependencies = depends t
    ; conflicts = conflicts t
    ; depopts = depopts t
    ; loc
    ; conflict_class = []
    ; pins = Name.Map.empty
    }
  | Some { file; contents = opam_file_string } ->
    let opam_file =
      Dune_pkg.Opam_file.read_from_string_exn
        ~contents:opam_file_string
        (Path.source file)
    in
    let convert_filtered_formula filtered_formula =
      Dune_pkg.Package_dependency.list_of_opam_filtered_formula loc filtered_formula
    in
    let dependencies = convert_filtered_formula (OpamFile.OPAM.depends opam_file) in
    let conflicts = convert_filtered_formula (OpamFile.OPAM.conflicts opam_file) in
    let depopts = convert_filtered_formula (OpamFile.OPAM.depopts opam_file) in
    let conflict_class =
      OpamFile.OPAM.conflict_class opam_file
      |> List.map ~f:Dune_pkg.Package_name.of_opam_package_name
    in
    let pins =
      match
        OpamFile.OPAM.pin_depends opam_file
        |> List.map ~f:(fun (pkg, url) ->
          let name = Dune_pkg.Package_name.of_opam_package_name (OpamPackage.name pkg) in
          let version =
            Dune_pkg.Package_version.of_opam_package_version (OpamPackage.version pkg)
          in
          let loc = Loc.in_file (Path.source file) in
          ( name
          , { Dune_pkg.Local_package.loc; version; url = loc, url; name; origin = `Opam }
          ))
        |> Name.Map.of_list
      with
      | Ok x -> x
      | Error (_, pkg, _) ->
        User_error.raise
          ~loc:pkg.loc
          [ Pp.textf "package %s is already pinned" (Name.to_string pkg.name) ]
    in
    { Dune_pkg.Local_package.name = name t
    ; version
    ; dependencies
    ; conflicts
    ; depopts
    ; loc
    ; conflict_class
    ; pins
    }
;;
