open Import
open Dune_lang.Decoder

module Names : sig
  type t

  val names : t -> (Loc.t * string) Nonempty_list.t
  val package : t -> Package.t option
  val has_public_name : t -> bool

  val make
    :  multi:bool
    -> allow_omit_names_version:Dune_lang.Syntax.Version.t
    -> (t, fields) Dune_lang.Decoder.parser

  val install_conf
    :  t
    -> ext:Filename.Extension.t
    -> enabled_if:Blang.t
    -> dir:Path.Source.t
    -> Install_conf.t option
end = struct
  type public =
    { public_names : (Loc.t * string option) list
    ; package : Package.t
    }

  type t =
    { names : (Loc.t * string) Nonempty_list.t
    ; public : public option
    ; dune_syntax : Syntax.Version.t
    }

  let names t = t.names
  let package t = Option.map t.public ~f:(fun p -> p.package)
  let has_public_name t = Option.is_some t.public

  let public_name ~dash_is_none =
    let+ loc, s = located string in
    ( loc
    , match s with
      | "-" -> if dash_is_none then None else Some s
      | s -> Some s )
  ;;

  let multi_fields =
    map_validate
      (let+ names = field_o "names" (repeat1 (located string))
       and+ pub_names =
         field_o "public_names" (repeat1 (public_name ~dash_is_none:true))
       in
       names, pub_names)
      ~f:(fun (names, public_names) ->
        match names, public_names with
        | Some names, Some public_names ->
          if List.length public_names = List.length names
          then Ok (Some names, Some public_names)
          else
            Error
              (User_error.make
                 [ Pp.text
                     "The list of public names must be of the same length as the list of \
                      names"
                 ])
        | names, public_names -> Ok (names, public_names))
  ;;

  let single_fields =
    let* dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
    let dash_is_none = dune_syntax >= (3, 8) in
    let+ name = field_o "name" (located string)
    and+ public_name = field_o "public_name" (public_name ~dash_is_none) in
    Option.map name ~f:List.singleton, Option.map public_name ~f:List.singleton
  ;;

  let pluralize s ~multi = if multi then s ^ "s" else s

  let make ~multi ~allow_omit_names_version =
    let check_valid_name_version = 3, 0 in
    let+ names = if multi then multi_fields else single_fields
    and+ loc = loc
    and+ dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax
    and+ package =
      field_o
        "package"
        (let+ loc = loc
         and+ pkg = Stanza_common.Pkg.decode in
         loc, pkg)
    and+ project = Dune_project.get_exn () in
    let names, public_names = names in
    let names =
      let names =
        let open Dune_lang.Syntax.Version.Infix in
        if dune_syntax >= check_valid_name_version
        then
          Option.iter
            names
            ~f:
              (List.iter ~f:(fun name ->
                 ignore (Module_name.parse_string_exn name : Module_name.t)));
        match names, public_names with
        | Some names, _ -> names
        | None, Some public_names ->
          if dune_syntax >= allow_omit_names_version
          then (
            let check_names = dune_syntax >= check_valid_name_version in
            List.map public_names ~f:(fun (loc, p) ->
              match p, check_names with
              | None, _ ->
                User_error.raise ~loc [ Pp.text "This executable must have a name field" ]
              | Some s, false -> loc, s
              | Some s, true ->
                (match Module_name.of_string_user_error (loc, s) with
                 | Ok _ -> loc, s
                 | Error user_message ->
                   User_error.raise
                     ~loc
                     [ Pp.textf "Invalid module name."
                     ; Pp.text
                         "Public executable names don't have this restriction. You can \
                          either change this public name to be a valid module name or \
                          add a \"name\" field with a valid module name."
                     ]
                     ~hints:(Module_name.valid_format_doc :: user_message.hints))))
          else
            User_error.raise
              ~loc
              [ Pp.textf
                  "%s field may not be omitted before dune version %s"
                  (pluralize ~multi "name")
                  (Dune_lang.Syntax.Version.to_string allow_omit_names_version)
              ]
        | None, None ->
          if dune_syntax >= allow_omit_names_version
          then
            User_error.raise
              ~loc
              [ Pp.textf
                  "either the %s or the %s field must be present"
                  (pluralize ~multi "name")
                  (pluralize ~multi "public_name")
              ]
          else
            User_error.raise
              ~loc
              [ Pp.textf "field %s is missing" (pluralize ~multi "name") ]
      in
      Nonempty_list.of_list names |> Option.value_exn
    in
    let public =
      match package, public_names with
      | None, None -> None
      | Some (_loc, package), Some public_names -> Some { package; public_names }
      | None, Some public_names ->
        if List.for_all public_names ~f:(fun (_, x) -> Option.is_none x)
        then None
        else
          Some
            { public_names
            ; package =
                Stanza_common.Pkg.default_exn ~loc project (pluralize "executable" ~multi)
            }
      | Some (loc, _), None ->
        User_error.raise
          ~loc
          [ Pp.textf
              "This field is useless without a (%s ...) field."
              (pluralize "public_name" ~multi)
          ]
    in
    { names; public; dune_syntax }
  ;;

  let install_conf t ~ext ~enabled_if ~dir =
    Option.map t.public ~f:(fun { package; public_names } ->
      let files =
        List.map2
          (Nonempty_list.to_list t.names)
          public_names
          ~f:(fun (locn, name) (locp, pub) ->
            Option.map pub ~f:(fun pub ->
              Install_entry.File.of_file_binding
                (File_binding.Unexpanded.make
                   ~src:(locn, name ^ ext)
                   ~dst:(locp, pub)
                   ~dune_syntax:t.dune_syntax
                   ~dir:(Some dir))))
        |> List.filter_opt
      in
      let loc =
        match public_names with
        | [] -> assert false
        | (loc, _) :: _ -> loc
      in
      { Install_conf.section = loc, Section Bin
      ; files
      ; dirs = []
      ; package
      ; enabled_if
      ; source_trees = []
      })
  ;;
end

module Link_mode = struct
  module T = struct
    type t =
      | Byte_complete
      | Other of
          { mode : Mode_conf.t
          ; kind : Binary_kind.t
          }

    let compare a b =
      match a, b with
      | Byte_complete, Byte_complete -> Eq
      | Byte_complete, _ -> Lt
      | _, Byte_complete -> Gt
      | Other { mode; kind }, Other t ->
        let open Ordering.O in
        let= () = Mode_conf.compare mode t.mode in
        Binary_kind.compare kind t.kind
    ;;

    let to_dyn = Dyn.opaque
  end

  include T

  let make mode kind = Other { mode; kind }
  let exe = make Best Exe
  let object_ = make Best Object
  let shared_object = make Best Shared_object
  let byte = make Byte Exe
  let native = make Native Exe
  let js = make Byte Js
  let plugin = make Best Plugin

  let simple_representations =
    [ "exe", exe
    ; "object", object_
    ; "shared_object", shared_object
    ; "byte", byte
    ; "native", native
    ; "js", js
    ; "byte_complete", Byte_complete
    ; "plugin", plugin
    ]
  ;;

  let simple = Dune_lang.Decoder.enum simple_representations

  let decode =
    enter
      (let+ mode = Mode_conf.decode
       and+ kind = Binary_kind.decode in
       make mode kind)
    <|> simple
  ;;

  let simple_encode link_mode =
    let is_ok (_, candidate) = compare candidate link_mode = Eq in
    List.find ~f:is_ok simple_representations
    |> Option.map ~f:(fun (s, _) -> Dune_lang.atom s)
  ;;

  let encode link_mode =
    match simple_encode link_mode with
    | Some s -> s
    | None ->
      (match link_mode with
       | Byte_complete -> assert false
       | Other { mode; kind } ->
         Dune_lang.Encoder.pair Mode_conf.encode Binary_kind.encode (mode, kind))
  ;;

  let to_dyn t =
    match t with
    | Byte_complete -> Dyn.Variant ("Byte_complete", [])
    | Other { mode; kind } ->
      let open Dyn in
      Variant
        ( "Other"
        , [ record [ "mode", Mode_conf.to_dyn mode; "kind", Binary_kind.to_dyn kind ] ] )
  ;;

  let extension t ~loc ~ext_obj ~ext_dll =
    match t with
    | Byte_complete -> ".bc.exe"
    | Other { mode; kind } ->
      let same_as_mode : Mode.t =
        match mode with
        | Byte -> Byte
        | Native | Best ->
          (* From the point of view of the extension, [native] and [best] are
             the same *)
          Native
      in
      (match same_as_mode, kind with
       | Byte, C -> ".bc.c"
       | Native, C ->
         User_error.raise ~loc [ Pp.text "C file generation only supports bytecode!" ]
       | Byte, Exe -> ".bc"
       | Native, Exe -> ".exe"
       | Byte, Object -> ".bc" ^ ext_obj
       | Native, Object -> ".exe" ^ ext_obj
       | Byte, Shared_object -> ".bc" ^ ext_dll
       | Native, Shared_object -> ext_dll
       | mode, Plugin -> Mode.plugin_ext mode
       | Byte, Js -> Js_of_ocaml.Ext.exe
       | Native, Js ->
         User_error.raise ~loc [ Pp.text "Javascript generation only supports bytecode!" ])
  ;;

  module O = Comparable.Make (T)

  let installable_modes =
    [ (0, 0), exe; (0, 0), native; (0, 0), byte; (3, 6), Byte_complete ]
  ;;

  module Map = struct
    include O.Map

    let decode =
      let+ loc, l = located (repeat (located decode)) in
      match l with
      | [] -> User_error.raise ~loc [ Pp.textf "No linking mode defined" ]
      | l ->
        let t =
          List.fold_left l ~init:empty ~f:(fun acc (loc, link_mode) ->
            set acc link_mode loc)
        in
        (match
           String.Map.of_list_map (to_list t) ~f:(fun (lm, loc) ->
             extension lm ~loc ~ext_obj:".OBJ" ~ext_dll:".DLL", lm)
         with
         | Ok _ -> ()
         | Error (_ext, (lm1, _), (lm2, _)) ->
           User_error.raise
             ~loc
             [ Pp.textf
                 "It is not allowed use both %s and %s together as they use the same \
                  file extension."
                 (Dune_lang.to_string (encode lm1))
                 (Dune_lang.to_string (encode lm2))
             ]);
        t
    ;;

    let byte_and_exe = of_list_exn [ byte, Loc.none; exe, Loc.none ]

    let default_for_exes ~version =
      if version < (2, 0) then byte_and_exe else singleton exe Loc.none
    ;;

    let default_for_tests ~version =
      if version < (3, 0) then byte_and_exe else singleton exe Loc.none
    ;;

    let best_install_mode t ~(dune_version : Syntax.Version.t) =
      let rec loop acc = function
        | [] -> acc
        | (since, mode) :: rest ->
          (match mem t mode with
           | false -> loop acc rest
           | true ->
             if dune_version < since
             then loop (Some (`Unavailable_until (since, mode))) rest
             else Some (`Found mode))
      in
      match loop None installable_modes with
      | None -> None
      | Some (`Found f) -> Some f
      | Some (`Unavailable_until (since, mode)) ->
        let what =
          List.find_map simple_representations ~f:(fun (rep, mode') ->
            Option.some_if (Ordering.is_eq (T.compare mode mode')) rep)
          |> Option.value_exn
        in
        let loc = find_exn t mode in
        Syntax.Error.since loc Stanza.syntax since ~what
    ;;
  end
end

type t =
  { names : (Loc.t * string) Nonempty_list.t
  ; link_flags : Link_flags.Spec.t
  ; link_deps : Dep_conf.t list
  ; modes : Loc.t Link_mode.Map.t
  ; optional : bool
  ; buildable : Buildable.t
  ; package : Package.t option
  ; promote : Rule.Promote.t option
  ; install_conf : Install_conf.t option
  ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
  ; forbidden_libraries : (Loc.t * Lib_name.t) list
  ; bootstrap_info : string option
  ; enabled_if : Blang.t
  ; dune_version : Dune_lang.Syntax.Version.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let bootstrap_info_extension =
  let syntax =
    Dune_lang.Syntax.create
      ~name:"dune-bootstrap-info"
      ~desc:"private extension to handle Dune bootstrap"
      [ (0, 1), `Since (2, 0) ]
  in
  Dune_project.Extension.register syntax (return ((), [])) Dyn.unit
;;

let common =
  let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
  let+ buildable = Buildable.decode Executable
  and+ (_ : bool) =
    field
      "link_executables"
      ~default:true
      (Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0) >>> bool)
  and+ link_deps = field "link_deps" (repeat Dep_conf.decode) ~default:[]
  and+ link_flags = Link_flags.Spec.decode ~check:None
  and+ modes =
    field
      "modes"
      Link_mode.Map.decode
      ~default:(Link_mode.Map.default_for_exes ~version:dune_version)
  and+ optional = field_b "optional" ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 0))
  and+ promote =
    field_o
      "promote"
      (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> Rule_mode_decoder.Promote.decode)
  and+ () =
    map_validate
      (field "inline_tests" (repeat junk >>| fun _ -> true) ~default:false)
      ~f:(function
        | false -> Ok ()
        | true ->
          Error
            (User_error.make
               [ Pp.text "Inline tests are only allowed in libraries."
               ; Pp.text "See https://github.com/ocaml/dune/issues/745 for more details."
               ]))
  and+ embed_in_plugin_libraries =
    field_o
      "embed_in_plugin_libraries"
      (Dune_lang.Syntax.since Stanza.syntax (2, 4)
       >>> located (repeat (located Lib_name.decode)))
  and+ forbidden_libraries =
    field
      "forbidden_libraries"
      (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> repeat (located Lib_name.decode))
      ~default:[]
  and+ bootstrap_info =
    field_o
      "bootstrap_info"
      (let+ loc = loc
       and+ fname = filename
       and+ project = Dune_project.get_exn () in
       if not (Dune_project.is_extension_set project bootstrap_info_extension)
       then User_error.raise ~loc [ Pp.text "This field is reserved for Dune itself" ];
       fname)
  and+ project_root = Dune_project.get_exn () >>| Dune_project.root
  and+ enabled_if =
    let allowed_vars = Enabled_if.common_vars ~since:(2, 3) in
    let is_error = Dune_lang.Syntax.Version.Infix.(dune_version >= (2, 6)) in
    Enabled_if.decode ~allowed_vars ~is_error ~since:(Some (2, 3)) ()
  in
  fun names ~multi ->
    let has_public_name = Names.has_public_name names in
    let private_names = Names.names names in
    let install_conf =
      match Link_mode.Map.best_install_mode ~dune_version modes with
      | None when has_public_name ->
        User_error.raise
          ~loc:buildable.loc
          [ Pp.textf
              "No installable mode found for %s."
              (if multi then "these executables" else "this executable")
          ; Pp.text "When public_name is set, one of the following modes is required:"
          ; Pp.enumerate
              (List.filter_map Link_mode.installable_modes ~f:(fun (since, mode) ->
                 Option.some_if (dune_version >= since) mode))
              ~f:(fun mode -> Pp.verbatim (Dune_lang.to_string (Link_mode.encode mode)))
          ]
      | None -> None
      | Some mode ->
        let ext =
          match mode with
          | Byte_complete -> ".bc.exe"
          | Other { mode = Byte; _ } -> ".bc"
          | Other { mode = Native | Best; _ } -> ".exe"
        in
        Names.install_conf names ~ext ~enabled_if ~dir:project_root
    in
    let embed_in_plugin_libraries =
      let plugin =
        Link_mode.Map.existsi modes ~f:(fun mode _ ->
          match mode with
          | Link_mode.Other { kind = Plugin; _ } -> true
          | _ -> false)
      in
      match embed_in_plugin_libraries, plugin with
      | None, _ -> []
      | Some (_, l), true -> l
      | Some (loc, _), false ->
        User_error.raise
          ~loc
          [ Pp.textf "This field can only be used when linking a plugin." ]
    in
    { names = private_names
    ; link_flags
    ; link_deps
    ; modes
    ; optional
    ; buildable
    ; package = Names.package names
    ; promote
    ; install_conf
    ; embed_in_plugin_libraries
    ; forbidden_libraries
    ; bootstrap_info
    ; enabled_if
    ; dune_version
    }
;;

let single, multi =
  let make multi =
    fields
      (let+ names = Names.make ~multi ~allow_omit_names_version:(1, 1)
       and+ f = common in
       f names ~multi)
  in
  make false, make true
;;

let has_foreign t = Buildable.has_foreign t.buildable
let has_foreign_cxx t = Buildable.has_foreign_cxx t.buildable

let obj_dir t ~dir =
  let name = snd (Nonempty_list.hd t.names) in
  Obj_dir.make_exe ~dir ~name
;;
