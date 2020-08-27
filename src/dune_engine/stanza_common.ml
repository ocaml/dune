open Import
open Dune_lang.Decoder

(* Parse and resolve "package" fields *)
module Pkg = struct
  let listing packages =
    let longest_pkg =
      String.longest_map packages ~f:(fun p ->
          Package.Name.to_string p.Package.name)
    in
    Pp.enumerate packages ~f:(fun pkg ->
        Printf.ksprintf Pp.verbatim "%-*s (because of %s)" longest_pkg
          (Package.Name.to_string pkg.Package.name)
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
                    ( Package.Name.Map.keys packages
                    |> List.map ~f:Package.Name.to_string )))

  let decode =
    let+ p = Dune_project.get_exn ()
    and+ loc, name = located Package.Name.decode in
    match resolve p name with
    | Ok x -> x
    | Error e -> raise (User_error.E { e with loc = Some loc })

  let field stanza =
    map_validate
      (let+ p = Dune_project.get_exn ()
       and+ pkg = field_o "package" string in
       (p, pkg))
      ~f:(fun (p, pkg) ->
        match pkg with
        | None -> default p stanza
        | Some name -> resolve p (Package.Name.of_string name))
end

let modules_field name = Ordered_set_lang.field name

module Include = struct
  type context =
    { current_file : Path.Source.t
    ; include_stack : (Loc.t * Path.Source.t) list
    }

  let in_file file = { current_file = file; include_stack = [] }

  let error { current_file = file; include_stack } =
    let last, rest =
      match include_stack with
      | [] -> assert false
      | last :: rest -> (last, rest)
    in
    let loc = fst (Option.value (List.last rest) ~default:last) in
    let line_loc (loc, file) =
      sprintf "%s:%d"
        (Path.Source.to_string_maybe_quoted file)
        loc.Loc.start.pos_lnum
    in
    User_error.raise ~loc
      [ Pp.text "Recursive inclusion of dune files detected:"
      ; Pp.textf "File %s is included from %s"
          (Path.Source.to_string_maybe_quoted file)
          (line_loc last)
      ; Pp.vbox
          (Pp.concat_map rest ~sep:Pp.cut ~f:(fun x ->
               Pp.box ~indent:3
                 (Pp.seq (Pp.verbatim "-> ")
                    (Pp.textf "included from %s" (line_loc x)))))
      ]

  let load_sexps ~context:{ current_file; include_stack } (loc, fn) =
    let include_stack = (loc, current_file) :: include_stack in
    let dir = Path.Source.parent_exn current_file in
    let current_file = Path.Source.relative dir fn in
    if not (Path.exists (Path.source current_file)) then
      User_error.raise ~loc
        [ Pp.textf "File %s doesn't exist."
            (Path.Source.to_string_maybe_quoted current_file)
        ];
    if
      List.exists include_stack ~f:(fun (_, f) ->
          Path.Source.equal f current_file)
    then
      error { current_file; include_stack };
    let sexps =
      Dune_lang.Parser.load ~lexer:Dune_lang.Lexer.token
        (Path.source current_file) ~mode:Many
    in
    (sexps, { current_file; include_stack })
end
