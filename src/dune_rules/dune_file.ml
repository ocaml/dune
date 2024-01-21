open Import
open Dune_lang.Decoder

(* This file defines Dune types as well as the S-expression syntax for the
   various supported versions of the specification. *)

(* Deprecated *)
module Jbuild_version = struct
  type t = V1

  let decode = enum [ "1", V1 ]
end

let () =
  Dune_project.Extension.register_deleted ~name:"library_variants" ~deleted_in:(2, 6)
;;

module Executables = struct
  module Names : sig
    type t

    val names : t -> (Loc.t * string) list
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
      { names : (Loc.t * string) list
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
                       "The list of public names must be of the same length as the list \
                        of names"
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
                  Stanza_common.Pkg.default_exn
                    ~loc
                    project
                    (pluralize "executable" ~multi)
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
          List.map2 t.names public_names ~f:(fun (locn, name) (locp, pub) ->
            Option.map pub ~f:(fun pub ->
              Install_entry.File.of_file_binding
                (File_binding.Unexpanded.make
                   ~src:(locn, name ^ ext)
                   ~dst:(locp, pub)
                   ~dune_syntax:t.dune_syntax
                   ~dir:(Some dir))))
          |> List.filter_opt
        in
        { Install_conf.section = Section Bin
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
          , [ record [ "mode", Mode_conf.to_dyn mode; "kind", Binary_kind.to_dyn kind ] ]
          )
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
           User_error.raise
             ~loc
             [ Pp.text "Javascript generation only supports bytecode!" ])
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
    { names : (Loc.t * string) list
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
    and+ optional =
      field_b "optional" ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 0))
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
                 ; Pp.text
                     "See https://github.com/ocaml/dune/issues/745 for more details."
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
  let obj_dir t ~dir = Obj_dir.make_exe ~dir ~name:(snd (List.hd t.names))
end

module Tests = struct
  type t =
    { exes : Executables.t
    ; locks : Locks.t
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; build_if : Blang.t
    ; action : Dune_lang.Action.t option
    }

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let gen_parse names =
    fields
      (let* deps =
         field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
       in
       String_with_vars.add_user_vars_to_decoding_env
         (Bindings.var_names deps)
         (let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
          let+ buildable = Buildable.decode Executable
          and+ link_flags = Link_flags.Spec.decode ~check:None
          and+ names = names
          and+ package = field_o "package" Stanza_common.Pkg.decode
          and+ locks = Locks.field ()
          and+ modes =
            field
              "modes"
              Executables.Link_mode.Map.decode
              ~default:(Executables.Link_mode.Map.default_for_tests ~version:dune_version)
          and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
          and+ action =
            field_o
              "action"
              (Dune_lang.Syntax.since ~fatal:false Stanza.syntax (1, 2)
               >>> Dune_lang.Action.decode_dune_file)
          and+ forbidden_libraries =
            field
              "forbidden_libraries"
              (Dune_lang.Syntax.since Stanza.syntax (2, 0)
               >>> repeat (located Lib_name.decode))
              ~default:[]
          and+ build_if =
            field
              "build_if"
              ~default:Blang.true_
              (Syntax.since Stanza.syntax (3, 9)
               >>> Enabled_if.decode_value ~allowed_vars:Any ())
          in
          { exes =
              { Executables.link_flags
              ; link_deps = []
              ; modes
              ; optional = false
              ; buildable
              ; names
              ; package = None
              ; promote = None
              ; install_conf = None
              ; embed_in_plugin_libraries = []
              ; forbidden_libraries
              ; bootstrap_info = None
              ; enabled_if
              ; dune_version
              }
          ; locks
          ; package
          ; deps
          ; enabled_if
          ; build_if
          ; action
          }))
  ;;

  let multi = gen_parse (field "names" (repeat1 (located string)))
  let single = gen_parse (field "name" (located string) >>| List.singleton)
end

module Documentation = struct
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let decode =
    fields
      (let+ package = Stanza_common.Pkg.field ~stanza:"documentation"
       and+ mld_files = Ordered_set_lang.field "mld_files"
       and+ loc = loc in
       { loc; package; mld_files })
  ;;
end

module Include_subdirs = struct
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification

  type stanza = Loc.t * t

  include Stanza.Make (struct
      type nonrec t = stanza

      include Poly
    end)

  let decode ~enable_qualified =
    sum
      [ "no", return No
      ; "unqualified", return (Include Unqualified)
      ; ( "qualified"
        , let+ () =
            if enable_qualified then return () else Syntax.since Stanza.syntax (3, 7)
          in
          Include Qualified )
      ]
  ;;
end

module Library_redirect = struct
  type 'old_name t =
    { project : Dune_project.t
    ; loc : Loc.t
    ; old_name : 'old_name
    ; new_public_name : Loc.t * Lib_name.t
    }

  module Local = struct
    type nonrec t = (Loc.t * Lib_name.Local.t) t

    include Stanza.Make (struct
        type nonrec t = t

        include Poly
      end)

    let for_lib (lib : Library.t) ~new_public_name ~loc : t =
      { loc; new_public_name; old_name = lib.name; project = lib.project }
    ;;

    let of_private_lib (lib : Library.t) : t option =
      match lib.visibility with
      | Public _ | Private None -> None
      | Private (Some package) ->
        let loc, name = lib.name in
        let package_name = Package.name package in
        let new_public_name = loc, Lib_name.mangled package_name name in
        Some (for_lib lib ~loc ~new_public_name)
    ;;

    let of_lib (lib : Library.t) : t option =
      let open Option.O in
      let* public_name =
        match lib.visibility with
        | Public plib -> Some plib.name
        | Private _ -> None
      in
      if Lib_name.equal (Lib_name.of_local lib.name) (snd public_name)
      then None
      else (
        let loc = fst public_name in
        Some (for_lib lib ~loc ~new_public_name:public_name))
    ;;
  end
end

module Deprecated_library_name = struct
  module Old_name = struct
    type deprecation =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t = Public_lib.t * deprecation

    let decode =
      let+ public = Public_lib.decode ~allow_deprecated_names:true in
      let deprecation =
        let deprecated_package = Lib_name.package_name (Public_lib.name public) in
        if let name = Package.name (Public_lib.package public) in
           Package.Name.equal deprecated_package name
        then Not_deprecated
        else Deprecated { deprecated_package }
      in
      public, deprecation
    ;;
  end

  type t = Old_name.t Library_redirect.t

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let old_public_name (t : t) = Public_lib.name (fst t.old_name)

  let decode =
    fields
      (let+ loc = loc
       and+ project = Dune_project.get_exn ()
       and+ old_name = field "old_public_name" Old_name.decode
       and+ new_public_name = field "new_public_name" (located Lib_name.decode) in
       let () =
         let loc, old_name = (fst old_name).name in
         if Lib_name.equal (snd new_public_name) old_name
         then
           User_error.raise
             ~loc
             [ Pp.text "old_public_name cannot be the same as the new_public_name" ]
       in
       { Library_redirect.loc; project; old_name; new_public_name })
  ;;
end

module Include = struct
  type t = Loc.t * string

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)
end

module Stanzas = struct
  type t = Stanza.t list

  let rules l = List.map l ~f:(fun x -> Rule_conf.make_stanza x)
  let execs exe = [ Executables.make_stanza exe ]

  type constructors = Stanza.Parser.t list

  let stanzas : constructors =
    List.concat
      [ Site_stanzas.all
      ; Cram_stanza.stanza
      ; [ ( "library"
          , let+ x = Library.decode in
            let base = [ Library.make_stanza x ] in
            match Library_redirect.Local.of_lib x with
            | None -> base
            | Some r -> Library_redirect.Local.make_stanza r :: base )
        ; ( "foreign_library"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
            and+ x = Foreign.Library.decode in
            [ Foreign.Library.make_stanza x ] )
        ; "executable", Executables.single >>| execs
        ; "executables", Executables.multi >>| execs
        ; ( "rule"
          , let+ loc = loc
            and+ x = Rule_conf.decode in
            [ Rule_conf.make_stanza { x with loc } ] )
        ; ( "ocamllex"
          , let+ loc = loc
            and+ x = Rule_conf.ocamllex in
            rules (Rule_conf.ocamllex_to_rule loc x) )
        ; ( "ocamlyacc"
          , let+ loc = loc
            and+ x = Rule_conf.ocamlyacc in
            rules (Rule_conf.ocamlyacc_to_rule loc x) )
        ; ( "install"
          , let+ x = Install_conf.decode in
            [ Install_conf.make_stanza x ] )
        ; ( "alias"
          , let+ x = Alias_conf.decode in
            [ Alias_conf.make_stanza x ] )
        ; ( "copy_files"
          , let+ x = Copy_files.decode in
            [ Copy_files.make_stanza x ] )
        ; ( "copy_files#"
          , let+ x = Copy_files.decode in
            [ Copy_files.make_stanza { x with add_line_directive = true } ] )
        ; ( "include"
          , let+ loc = loc
            and+ fn = relative_file in
            [ Include.make_stanza (loc, fn) ] )
        ; ( "documentation"
          , let+ d = Documentation.decode in
            [ Documentation.make_stanza d ] )
        ; ( "jbuild_version"
          , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0)
            and+ _ = Jbuild_version.decode in
            [] )
        ; ( "tests"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
            and+ t = Tests.multi in
            [ Tests.make_stanza t ] )
        ; ( "test"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
            and+ t = Tests.single in
            [ Tests.make_stanza t ] )
        ; ( "external_variant"
          , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (2, 6) in
            [] )
        ; ( "env"
          , let+ x = Dune_env.decode in
            [ Dune_env.make_stanza x ] )
        ; ( "include_subdirs"
          , let* project = Dune_project.get_exn () in
            let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
            and+ t =
              let enable_qualified =
                Dune_project.is_extension_set project Coq_stanza.key
              in
              Include_subdirs.decode ~enable_qualified
            and+ loc = loc in
            [ Include_subdirs.make_stanza (loc, t) ] )
        ; ( "toplevel"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 7)
            and+ t = Toplevel_stanza.decode in
            [ Toplevel_stanza.make_stanza t ] )
        ; ( "deprecated_library_name"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
            and+ t = Deprecated_library_name.decode in
            [ Deprecated_library_name.make_stanza t ] )
        ]
      ]
  ;;

  let () = Dune_project.Lang.register Stanza.syntax stanzas

  let parser project =
    let syntax_parser = Dune_project.stanza_parser project in
    Dune_project.set project syntax_parser
  ;;

  let parse parser = Dune_lang.Decoder.parse parser Univ_map.empty

  let of_ast (project : Dune_project.t) sexp =
    let parser = parser project in
    parse parser sexp
  ;;

  (* XXX this is needed for evaluating includes generated by dune files written
     in OCaml syntax.*)
  let rec parse_file_includes ~stanza_parser ~context sexps =
    List.concat_map sexps ~f:(parse stanza_parser)
    |> Memo.List.concat_map ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Include.T (loc, fn) ->
        let open Memo.O in
        let* sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
        parse_file_includes ~stanza_parser ~context sexps
      | _ -> Memo.return [ stanza ])
  ;;

  let parse ~file ~dir (project : Dune_project.t) sexps =
    let stanza_parser = parser project in
    let warnings = Warning_emit.Bag.create () in
    let stanza_parser = Warning_emit.Bag.set warnings stanza_parser in
    let open Memo.O in
    let* stanzas =
      let context =
        Include_stanza.in_file
        @@
        match file with
        | Some f -> f
        | None ->
          (* TODO this is wrong *)
          Path.Source.relative dir Source_tree.Dune_file.fname
      in
      parse_file_includes ~stanza_parser ~context sexps
    in
    let (_ : bool) =
      List.fold_left stanzas ~init:false ~f:(fun env stanza ->
        match Stanza.repr stanza with
        | Dune_env.T e ->
          if env
          then
            User_error.raise
              ~loc:e.loc
              [ Pp.text "The 'env' stanza cannot appear more than once" ]
          else true
        | _ -> env)
    in
    let+ () = Warning_emit.Bag.emit_all warnings in
    stanzas
  ;;
end

let stanza_package stanza =
  match Stanza.repr stanza with
  | Library.T lib -> Library.package lib
  | Alias_conf.T { package = Some package; _ }
  | Rule_conf.T { package = Some package; _ }
  | Install_conf.T { package; _ }
  | Plugin.T { package; _ }
  | Executables.T { install_conf = Some { package; _ }; _ }
  | Documentation.T { package; _ }
  | Tests.T { package = Some package; _ } -> Some package
  | Coq_stanza.Theory.T { package = Some package; _ } -> Some package
  | _ -> None
;;

type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanzas.t
  }

let is_promoted_rule =
  let is_promoted_mode version = function
    | Rule.Mode.Promote { only = None; lifetime; _ } ->
      if version >= (3, 5)
      then (
        match lifetime with
        | Unlimited -> true
        | Until_clean -> false)
      else true
    | _ -> false
  in
  fun version rule ->
    match Stanza.repr rule with
    | Rule_conf.T { mode; _ } | Menhir_stanza.T { mode; _ } ->
      is_promoted_mode version mode
    | _ -> false
;;

let parse sexps ~dir ~file ~project =
  let open Memo.O in
  let+ stanzas = Stanzas.parse ~file ~dir project sexps in
  let stanzas =
    if !Clflags.ignore_promoted_rules
    then (
      let version = Dune_project.dune_version project in
      List.filter stanzas ~f:(fun s -> not (is_promoted_rule version s)))
    else stanzas
  in
  { dir; project; stanzas }
;;

module Make_fold (M : Monad.S) = struct
  open M.O

  let rec fold_stanzas l ~init ~f =
    match l with
    | [] -> M.return init
    | t :: l -> inner_fold t t.stanzas l ~init ~f

  and inner_fold t inner_list l ~init ~f =
    match inner_list with
    | [] -> fold_stanzas l ~init ~f
    | x :: inner_list ->
      let* init = f t x init in
      inner_fold t inner_list l ~init ~f
  ;;
end

module Memo_fold = Make_fold (Memo)
module Id_fold = Make_fold (Monad.Id)

let fold_stanzas t ~init ~f = Id_fold.fold_stanzas t ~init ~f

let equal t { dir; project; stanzas } =
  Path.Source.equal t.dir dir
  && Dune_project.equal t.project project
  && List.equal Stanza.equal t.stanzas stanzas
;;

let hash = Poly.hash
let to_dyn = Dyn.opaque
let of_ast = Stanzas.of_ast
