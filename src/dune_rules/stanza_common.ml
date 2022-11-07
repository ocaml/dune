open Import
open Dune_lang.Decoder

(* Parse and resolve "package" fields *)
module Pkg = struct
  let listing packages =
    let longest_pkg =
      String.longest_map packages ~f:(fun p ->
          let name = Package.name p in
          Package.Name.to_string name)
    in
    Pp.enumerate packages ~f:(fun pkg ->
        let name = Package.name pkg in
        Printf.ksprintf Pp.verbatim "%-*s (because of %s)" longest_pkg
          (Package.Name.to_string name)
          (Path.Source.to_string (Package.opam_file pkg)))

  let default (project : Dune_project.t) stanza =
    match Package.Name.Map.values (Dune_project.packages project) with
    | [ pkg ] -> Ok pkg
    | [] ->
      Error
        (User_error.make
           [ Pp.text
               "The current project defines some public elements, but no opam \
                packages are defined."
           ; Pp.text
               "Please add a <package>.opam file at the project root so that \
                these elements are installed into it."
           ])
    | _ :: _ :: _ ->
      Error
        (User_error.make
           [ Pp.text
               "I can't determine automatically which package this stanza is \
                for."
           ; Pp.text "I have the choice between these ones:"
           ; listing (Package.Name.Map.values (Dune_project.packages project))
           ; Pp.textf
               "You need to add a (package ...) field to this (%s) stanza."
               stanza
           ])

  let default_exn ~loc project stanza =
    match default project stanza with
    | Ok p -> p
    | Error msg -> raise (User_error.E { msg with loc = Some loc })

  let resolve (project : Dune_project.t) name =
    let packages = Dune_project.packages project in
    match Package.Name.Map.find packages name with
    | Some pkg -> Ok pkg
    | None ->
      let name_s = Package.Name.to_string name in
      if Package.Name.Map.is_empty packages then
        Error
          (User_error.make
             [ Pp.text
                 "You cannot declare items to be installed without adding a \
                  <package>.opam file at the root of your project."
             ; Pp.textf
                 "To declare elements to be installed as part of package %S, \
                  add a %S file at the root of your project."
                 name_s
                 (Package.Name.opam_fn name)
             ; Pp.textf "Root of the project as discovered by dune: %s"
                 (Path.Source.to_string_maybe_quoted
                    (Dune_project.root project))
             ])
      else
        Error
          (User_error.make
             [ Pp.textf "The current scope doesn't define package %S." name_s
             ; Pp.text
                 "The only packages for which you can declare elements to be \
                  installed in this directory are:"
             ; listing (Package.Name.Map.values packages)
             ]
             ~hints:
               (User_message.did_you_mean name_s
                  ~candidates:
                    (Package.Name.Map.keys packages
                    |> List.map ~f:Package.Name.to_string)))

  let decode =
    let+ p = Dune_project.get_exn ()
    and+ loc, name = located Package.Name.decode in
    match resolve p name with
    | Ok x -> x
    | Error e -> raise (User_error.E { e with loc = Some loc })

  let field ~stanza =
    map_validate
      (let+ p = Dune_project.get_exn ()
       and+ pkg = field_o "package" Package.Name.decode in
       (p, pkg))
      ~f:(fun (p, pkg) ->
        match pkg with
        | None -> default p stanza
        | Some name -> resolve p name)

  let field_opt ?check () =
    let decode =
      let decode = Package.Name.decode in
      match check with
      | None -> decode
      | Some check -> check >>> decode
    in
    map_validate
      (let+ p = Dune_project.get_exn ()
       and+ pkg = field_o "package" decode in
       (p, pkg))
      ~f:(fun (p, pkg) ->
        match pkg with
        | None -> Ok None
        | Some name -> resolve p name |> Result.map ~f:Option.some)
end

let modules_field name = Ordered_set_lang.field name

let preprocess_fields =
  let+ preprocess =
    field "preprocess" Preprocess.Per_module.decode
      ~default:(Preprocess.Per_module.default ())
  and+ preprocessor_deps =
    field_o "preprocessor_deps"
      (let+ loc = loc
       and+ l = repeat Dep_conf.decode in
       (loc, l))
  and+ syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
  let preprocessor_deps =
    match preprocessor_deps with
    | None -> []
    | Some (loc, deps) ->
      let deps_might_be_used =
        Module_name.Per_item.exists preprocess ~f:(fun p ->
            match (p : _ Preprocess.t) with
            | Action _ | Pps _ -> true
            | No_preprocessing | Future_syntax _ -> false)
      in
      if not deps_might_be_used then
        User_warning.emit ~loc
          ~is_error:(syntax >= (2, 0))
          [ Pp.text
              "This preprocessor_deps field will be ignored because no \
               preprocessor that might use them is configured."
          ];
      deps
  in
  (preprocess, preprocessor_deps)

let instrumentation =
  located
    (multi_field "instrumentation"
       (Dune_lang.Syntax.since Stanza.syntax (2, 7)
       >>> fields
             (let+ backend =
                field "backend"
                  (let+ libname = located Lib_name.decode
                   and+ flags =
                     let* current_ver =
                       Dune_lang.Syntax.get_exn Stanza.syntax
                     in
                     let version_check flag =
                       let ver = (2, 8) in
                       if current_ver >= ver then flag
                       else
                         let what =
                           "The possibility to pass arguments to \
                            instrumentation backends"
                         in
                         Dune_lang.Syntax.Error.since
                           (String_with_vars.loc flag)
                           Stanza.syntax ver ~what
                     in
                     repeat (String_with_vars.decode >>| version_check)
                   in
                   (libname, flags))
              and+ deps =
                field "deps" ~default:[]
                  (Dune_lang.Syntax.since Stanza.syntax (2, 9)
                  >>> repeat Dep_conf.decode)
              in
              (backend, deps))))
