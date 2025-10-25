open Import

let listing packages =
  let longest_pkg =
    String.longest_map packages ~f:(fun p ->
      let name = Package.name p in
      Package.Name.to_string name)
  in
  Pp.enumerate packages ~f:(fun pkg ->
    let name = Package.name pkg in
    Printf.ksprintf
      Pp.verbatim
      "%-*s (because of %s)"
      longest_pkg
      (Package.Name.to_string name)
      (Path.Source.to_string (Package.opam_file pkg)))
;;

let default (project : Dune_project.t) stanza =
  let packages = Dune_project.including_hidden_packages project in
  match Package.Name.Map.values packages with
  | [ pkg ] -> Ok pkg
  | [] ->
    Error
      (User_error.make
         [ Pp.text
             "The current project defines some public elements, but no opam packages are \
              defined."
         ; Pp.text
             "Please add a <package>.opam file at the project root so that these \
              elements are installed into it."
         ])
  | _ :: _ :: _ ->
    Error
      (User_error.make
         [ Pp.text "I can't determine automatically which package this stanza is for."
         ; Pp.text "I have the choice between these ones:"
         ; listing (Package.Name.Map.values packages)
         ; Pp.textf "You need to add a (package ...) field to this (%s) stanza." stanza
         ])
;;

let default_exn ~loc project stanza =
  match default project stanza with
  | Ok p -> p
  | Error msg -> raise (User_error.E { msg with loc = Some loc })
;;

let resolve (project : Dune_project.t) mask (loc, name) =
  let packages = Dune_project.including_hidden_packages project in
  match Package.Name.Map.find packages name with
  | Some pkg ->
    (match Package_mask.validate mask ~loc (Package.id pkg) with
     | Ok () -> Ok pkg
     | Error e -> Error e)
  | None ->
    let name_s = Package.Name.to_string name in
    if Package.Name.Map.is_empty packages
    then
      Error
        (User_error.make
           ~loc
           [ Pp.text
               "You cannot declare items to be installed without adding a <package>.opam \
                file at the root of your project."
           ; Pp.textf
               "To declare elements to be installed as part of package %S, add a %S file \
                at the root of your project."
               name_s
               (Package.Name.opam_fn name)
           ; Pp.textf
               "Root of the project as discovered by dune: %s"
               (Path.Source.to_string_maybe_quoted (Dune_project.root project))
           ])
    else
      Error
        (User_error.make
           ~loc
           [ Pp.textf "The current scope doesn't define package %S." name_s
           ; Pp.text
               "The only packages for which you can declare elements to be installed in \
                this directory are:"
           ; listing (Package.Name.Map.values packages)
           ]
           ~hints:
             (User_message.did_you_mean
                name_s
                ~candidates:
                  (Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string)))
;;

let field_opt ?check project =
  let open Decoder in
  let decode =
    let decode = Package.Name.decode in
    match check with
    | None -> decode
    | Some check -> check >>> decode
  in
  let* mask = Package_mask.decode () in
  map_validate
    (field_o "package" (located decode))
    ~f:(fun pkg ->
      match pkg with
      | None -> Ok None
      | Some ((loc, _name) as name) ->
        resolve project mask name |> Result.map ~f:(fun p -> Some (loc, p)))
;;

let field ~stanza =
  let open Decoder in
  let* project = Dune_project.get_exn () in
  map_validate (field_opt project) ~f:(function
    | None -> default project stanza
    | Some (_, p) -> Ok p)
;;

let field_opt ?check () =
  let open Decoder in
  let* project = Dune_project.get_exn () in
  field_opt ?check project
;;
