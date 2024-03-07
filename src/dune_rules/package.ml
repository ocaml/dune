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
