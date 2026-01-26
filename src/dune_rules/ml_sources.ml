open Import
open Memo.O

module Origin = struct
  type t =
    | Library of Library.t
    | Executables of Executables.t
    | Tests of Tests.t
    | Melange of Melange_stanzas.Emit.t

  let loc = function
    | Library l -> l.buildable.loc
    | Executables e -> e.buildable.loc
    | Tests t -> t.exes.buildable.loc
    | Melange mel -> mel.loc
  ;;

  let preprocess = function
    | Library l -> l.buildable.preprocess
    | Executables e -> e.buildable.preprocess
    | Tests t -> t.exes.buildable.preprocess
    | Melange mel -> mel.preprocess
  ;;

  let to_dyn = function
    | Library _ -> Dyn.variant "Library" [ Dyn.Opaque ]
    | Executables _ -> Dyn.variant "Executables" [ Dyn.Opaque ]
    | Tests _ -> Dyn.variant "Tests" [ Dyn.Opaque ]
    | Melange _ -> Dyn.variant "Melange" [ Dyn.Opaque ]
  ;;
end

type parser_gen_dep_info =
  { deps : Path.Set.t
  ; targets : (Loc.t * Module.Source.t) Module_trie.t
  }

module Per_stanza = struct
  type component = Origin.t * Modules.t * Path.Build.t Obj_dir.t

  type t =
    { libraries : component Lib_id.Local.Map.t
    ; executables : component String.Map.t
    ; melange_emits : component String.Map.t
    ; ocamllexes : parser_gen_dep_info Loc.Map.t
    ; ocamlyaccs : parser_gen_dep_info Loc.Map.t
    ; menhirs : parser_gen_dep_info Loc.Map.t
    ; (* Map from modules to the origin they are part of *)
      rev_map : (Origin.t * Path.Build.t) list Module_name.Path.Map.t
    ; libraries_by_obj_dir : Lib_id.Local.t list Path.Build.Map.t
    }

  let empty =
    { libraries = Lib_id.Local.Map.empty
    ; executables = String.Map.empty
    ; melange_emits = String.Map.empty
    ; ocamllexes = Loc.Map.empty
    ; ocamlyaccs = Loc.Map.empty
    ; menhirs = Loc.Map.empty
    ; rev_map = Module_name.Path.Map.empty
    ; libraries_by_obj_dir = Path.Build.Map.empty
    }
  ;;

  type 'stanza group_part =
    { stanza : 'stanza
    ; sources : (Loc.t * Module.Source.t) Module_trie.t
    ; modules : Modules.t
    ; dir : Path.Build.t
    ; obj_dir : Path.Build.t Obj_dir.t
    }

  type 'stanza parser_gen_group =
    { stanza : 'stanza
    ; dep_info : parser_gen_dep_info
    }

  type groups =
    { libraries : Library.t group_part list
    ; executables : Executables.t group_part list
    ; tests : Tests.t group_part list
    ; melange_emits : Melange_stanzas.Emit.t group_part list
    ; ocamllexes : Parser_generators.t parser_gen_group list
    ; ocamlyaccs : Parser_generators.t parser_gen_group list
    ; menhirs : Menhir_stanza.t parser_gen_group list
    }

  let make
        { libraries = libs
        ; executables = exes
        ; tests
        ; melange_emits = emits
        ; ocamllexes
        ; ocamlyaccs
        ; menhirs
        }
    =
    let libraries, libraries_by_obj_dir =
      List.fold_left
        libs
        ~init:(Lib_id.Local.Map.empty, Path.Build.Map.empty)
        ~f:(fun (by_id, by_obj_dir) part ->
          let lib_id =
            let src_dir =
              Path.drop_optional_build_context_src_exn (Path.build part.dir)
            in
            Library.to_lib_id ~src_dir part.stanza
          in
          let by_id =
            let origin : Origin.t = Library part.stanza in
            Lib_id.Local.Map.add_exn by_id lib_id (origin, part.modules, part.obj_dir)
          and by_obj_dir =
            Path.Build.Map.update by_obj_dir (Obj_dir.obj_dir part.obj_dir) ~f:(function
              | None -> Some [ lib_id ]
              | Some lib_ids -> Some (lib_id :: lib_ids))
          in
          by_id, by_obj_dir)
    in
    let executables =
      let entries =
        List.concat
          [ List.map exes ~f:(fun (part : Executables.t group_part) ->
              let first_exe = snd (Nonempty_list.hd part.stanza.names) in
              let origin : Origin.t = Executables part.stanza in
              first_exe, (origin, part.modules, part.obj_dir, part.stanza.buildable.loc))
          ; List.map tests ~f:(fun (part : Tests.t group_part) ->
              let first_exe = snd (Nonempty_list.hd part.stanza.exes.names) in
              let origin : Origin.t = Tests part.stanza in
              ( first_exe
              , (origin, part.modules, part.obj_dir, part.stanza.exes.buildable.loc) ))
          ]
      in
      match String.Map.of_list entries with
      | Ok map ->
        String.Map.map map ~f:(fun (origin, modules, obj_dir, _loc) ->
          origin, modules, obj_dir)
      | Error (name, (_, _, _, loc1), (_, _, _, loc2)) ->
        User_error.raise
          ~loc:loc1
          [ Pp.textf "Executable %S appears for the second time in this directory" name
          ; Pp.textf "Already defined at %s" (Loc.to_file_colon_line loc2)
          ]
    in
    let melange_emits =
      match
        String.Map.of_list_map emits ~f:(fun part ->
          let origin : Origin.t = Melange part.stanza in
          part.stanza.target, (origin, part.modules, part.obj_dir))
      with
      | Ok x -> x
      | Error (name, _, part) ->
        User_error.raise
          ~loc:part.stanza.loc
          [ Pp.textf "Target %S appears for the second time in this directory" name ]
    in
    let ocamllexes =
      Loc.Map.of_list_map_exn ocamllexes ~f:(fun { stanza; dep_info } ->
        stanza.loc, dep_info)
    in
    let ocamlyaccs =
      Loc.Map.of_list_map_exn ocamlyaccs ~f:(fun { stanza; dep_info } ->
        stanza.loc, dep_info)
    in
    let menhirs =
      Loc.Map.of_list_map_exn menhirs ~f:(fun { stanza; dep_info } ->
        stanza.loc, dep_info)
    in
    let rev_map =
      let by_path (origin : Origin.t * Path.Build.t) trie =
        Module_trie.to_list_map trie ~f:(fun (_loc, m) -> Module.Source.path m, origin)
      in
      List.rev_concat
        [ List.rev_concat_map libs ~f:(fun part ->
            by_path (Library part.stanza, part.dir) part.sources)
        ; List.rev_concat_map exes ~f:(fun part ->
            by_path (Executables part.stanza, part.dir) part.sources)
        ; List.rev_concat_map tests ~f:(fun part ->
            by_path (Tests part.stanza, part.dir) part.sources)
        ; List.rev_concat_map emits ~f:(fun part ->
            by_path (Melange part.stanza, part.dir) part.sources)
        ]
      |> List.fold_left
           ~init:Module_name.Path.Map.empty
           ~f:(fun module_name_map (module_name, origin) ->
             Module_name.Path.Map.update module_name_map module_name ~f:(function
               | None -> Some [ origin ]
               | Some origins -> Some (origin :: origins)))
    in
    { libraries
    ; executables
    ; melange_emits
    ; ocamllexes
    ; ocamlyaccs
    ; menhirs
    ; rev_map
    ; libraries_by_obj_dir
    }
  ;;
end

type t =
  { modules : Per_stanza.t
  ; artifacts : Artifacts_obj.t Memo.Lazy.t
  ; include_subdirs : Include_subdirs.t
  }

let include_subdirs t = t.include_subdirs

let empty =
  { modules = Per_stanza.empty
  ; artifacts = Memo.Lazy.of_val Artifacts_obj.empty
  ; include_subdirs = No
  }
;;

let artifacts t = Memo.Lazy.force t.artifacts

let modules_of_files ~path ~dialects ~dir ~files =
  let dir = Path.build dir in
  let impl_files, intf_files =
    let make_module dialect name fn =
      name, Module.File.make dialect (Path.relative dir fn)
    in
    let loc = Loc.in_dir dir in
    Filename.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
      (* we aren't using Filename.extension because we want to ignore
         filenames such as `foo.cppo.ml` or `foo.{filter}.ml` (e.g. from the
         `(select ..)` field) *)
      match String.lsplit2 fn ~on:'.' with
      | None -> Skip
      | Some (s, ext) ->
        (match Dialect.DB.find_by_extension dialects ("." ^ ext) with
         | None -> Skip
         | Some (dialect, ml_kind) ->
           let module_ =
             let name = Module_name.of_string_allow_invalid (loc, s) in
             make_module dialect name fn
           in
           (match ml_kind with
            | Impl -> Left module_
            | Intf -> Right module_)))
  in
  let parse_one_set (files : (Module_name.Unchecked.t * Module.File.t) list) =
    match Module_name.Unchecked.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      User_error.raise
        ~loc:(Loc.in_dir dir)
        [ Pp.textf
            "Too many files for module %s in %s:"
            (Module_name.to_string (Module_name.Unchecked.allow_invalid name))
            (Path.Source.to_string_maybe_quoted src_dir)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f1))
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f2))
        ]
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module_name.Unchecked.Map.merge impls intfs ~f:(fun name impl intf ->
    Some
      (Module.Source.make
         Nonempty_list.(map (path @ [ name ]) ~f:Module_name.Unchecked.allow_invalid)
         ~impl
         ~intf))
;;

type for_ =
  | Library of Lib_id.Local.t
  | Exe of { first_exe : string }
  | Melange of { target : string }

let dyn_of_for_ =
  let open Dyn in
  function
  | Library n -> variant "Library" [ Lib_id.Local.to_dyn n ]
  | Exe { first_exe } -> variant "Exe" [ record [ "first_exe", string first_exe ] ]
  | Melange { target } -> variant "Melange" [ record [ "target", string target ] ]
;;

let raise_module_conflict_error ~module_path origins =
  let locs = List.map origins ~f:Origin.loc |> List.sort ~compare:Loc.compare in
  let main_message =
    Pp.textf
      "Module %S is used in several stanzas:"
      (Module_name.Path.to_string module_path)
  in
  let loc, related_locs =
    match locs with
    | [] ->
      (* duplicates imply at least at one module with this location *)
      assert false
    | loc :: related_locs -> loc, related_locs
  in
  let annots =
    let main = User_message.make ~loc [ main_message ] in
    let related =
      List.map related_locs ~f:(fun loc ->
        User_message.make ~loc [ Pp.text "Used in this stanza" ])
    in
    User_message.Annots.singleton
      Compound_user_error.annot
      [ Compound_user_error.make ~main ~related ]
  in
  User_error.raise
    ~annots
    ~loc:(Loc.drop_position loc)
    [ main_message
    ; Pp.enumerate locs ~f:(fun loc -> Pp.verbatim (Loc.to_file_colon_line loc))
    ; Pp.text
        "To fix this error, you must specify an explicit \"modules\" field in every \
         library, executable, and executables stanzas in this dune file. Note that each \
         module cannot appear in more than one \"modules\" field - it must belong to a \
         single library or executable."
    ]
;;

let find_origin (t : t) ~libs path =
  match Module_name.Path.Map.find t.modules.rev_map path with
  | None | Some [] -> Memo.return None
  | Some [ (origin, _) ] -> Memo.return (Some origin)
  | Some origins ->
    Memo.List.filter_map origins ~f:(fun (origin, dir) ->
      match origin with
      | Executables _ | Tests _ | Melange _ -> Memo.return (Some origin)
      | Library lib ->
        let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
        Lib.DB.available_by_lib_id libs (Local (Library.to_lib_id ~src_dir lib))
        >>| (function
         | false -> None
         | true -> Some origin))
    >>| (function
     | [] -> None
     | [ origin ] -> Some origin
     | origins -> raise_module_conflict_error origins ~module_path:path)
;;

let modules_and_obj_dir t ~libs ~for_ =
  match
    match for_ with
    | Library lib_id -> Lib_id.Local.Map.find t.modules.libraries lib_id
    | Exe { first_exe } -> String.Map.find t.modules.executables first_exe
    | Melange { target } -> String.Map.find t.modules.melange_emits target
  with
  | Some (Library _, modules, obj_dir) ->
    let* () =
      Modules.fold_user_written modules ~init:[] ~f:(fun m acc -> Module.path m :: acc)
      |> Memo.List.iter ~f:(fun module_path ->
        let+ (_origin : Origin.t option) = find_origin t ~libs module_path in
        ())
    in
    (match
       Path.Build.Map.find_exn t.modules.libraries_by_obj_dir (Obj_dir.obj_dir obj_dir)
     with
     | [] | [ _ ] -> Memo.return (modules, obj_dir)
     | lib_ids ->
       Memo.List.filter lib_ids ~f:(fun lib_id ->
         Lib.DB.available_by_lib_id libs (Local lib_id))
       >>| (function
        | [] | [ _ ] -> modules, obj_dir
        | lib_ids ->
          let lib_id =
            (* Get the 2nd loc *)
            List.sort lib_ids ~compare:(fun a b ->
              Loc.compare (Lib_id.Local.loc a) (Lib_id.Local.loc b))
            |> List.tl
            |> List.hd
          in
          User_error.raise
            ~loc:(Lib_id.Local.loc lib_id)
            [ Pp.textf
                "Library %S appears for the second time in this directory"
                (Lib_name.to_string (Lib_id.Local.name lib_id))
            ]))
  | Some (_, modules, obj_dir) -> Memo.return (modules, obj_dir)
  | None ->
    let map =
      match for_ with
      | Library _ ->
        Lib_id.Local.Map.keys t.modules.libraries |> Dyn.list Lib_id.Local.to_dyn
      | Exe _ -> String.Map.keys t.modules.executables |> Dyn.(list string)
      | Melange _ -> String.Map.keys t.modules.melange_emits |> Dyn.(list string)
    in
    Code_error.raise
      "modules_and_obj_dir: failed lookup"
      [ "keys", map; "for_", dyn_of_for_ for_ ]
;;

let modules t ~libs ~for_ = modules_and_obj_dir t ~libs ~for_ >>| fst

let virtual_modules ~lookup_vlib ~libs ~for_ vlib =
  let+ modules =
    match Lib_info.modules vlib ~for_ with
    | External modules ->
      Option.value_exn modules |> Modules.With_vlib.drop_vlib |> Memo.return
    | Local ->
      let src_dir = Lib_info.src_dir vlib |> Path.as_in_build_dir_exn in
      lookup_vlib ~dir:src_dir
      >>= modules ~libs ~for_:(Library (Lib_info.lib_id vlib |> Lib_id.to_local_exn))
  in
  let existing_virtual_modules = Modules.virtual_module_names modules in
  let allow_new_public_modules = Modules.wrapped modules |> Wrapped.to_bool |> not in
  { Modules_field_evaluator.Implementation.existing_virtual_modules
  ; allow_new_public_modules
  }
;;

module Parser_generators = struct
  module Stanzas = Parser_generators

  type dep_info = parser_gen_dep_info =
    { deps : Path.Set.t
    ; targets : (Loc.t * Module.Source.t) Module_trie.t
    }

  type for_ =
    | Ocamllex of Loc.t
    | Ocamlyacc of Loc.t
    | Menhir of Loc.t

  let dyn_of_for_ =
    let open Dyn in
    function
    | Ocamllex loc -> variant "Ocamllex" [ Loc.to_dyn loc ]
    | Ocamlyacc loc -> variant "Ocamlyacc" [ Loc.to_dyn loc ]
    | Menhir loc -> variant "Menhir" [ Loc.to_dyn loc ]
  ;;

  let modules t ~for_ =
    match
      match for_ with
      | Ocamllex loc -> Loc.Map.find t.modules.ocamllexes loc
      | Ocamlyacc loc -> Loc.Map.find t.modules.ocamlyaccs loc
      | Menhir loc -> Loc.Map.find t.modules.menhirs loc
    with
    | Some modules -> modules
    | None ->
      let map =
        match for_ with
        | Ocamllex _ -> t.modules.ocamllexes
        | Ocamlyacc _ -> t.modules.ocamlyaccs
        | Menhir _ -> t.modules.menhirs
      in
      Code_error.raise
        "Parser_generators.modules: failed lookup"
        [ "keys", Dyn.list Loc.to_dyn (Loc.Map.keys map); "for_", dyn_of_for_ for_ ]
  ;;

  module Targets = struct
    type for_ =
      | Ocamllex of Parser_generators.t
      | Ocamlyacc of Parser_generators.t
      | Menhir of Menhir_stanza.t

    let modules ~for_ =
      match for_ with
      | Menhir { Menhir_stanza.modules; _ }
      | Ocamllex { Parser_generators.modules; _ }
      | Ocamlyacc { Parser_generators.modules; _ } -> modules
    ;;

    let extension ~for_ =
      match for_ with
      | Ocamllex _ -> ".mll"
      | Ocamlyacc _ | Menhir _ -> ".mly"
    ;;
  end

  let expand_modules =
    let make_file ~original_path ~ml_kind =
      let ext = Dialect.extension Dialect.ocaml ml_kind |> Option.value_exn in
      let path = Path.set_extension original_path ~ext in
      Module.File.make ~original_path Dialect.ocaml path
    in
    let make_module ~module_path ~original_path ~for_ =
      let impl = Some (make_file ~original_path ~ml_kind:Ml_kind.Impl) in
      let intf =
        match for_ with
        | Targets.Ocamllex _ -> None
        | Ocamlyacc _ | Menhir _ ->
          let intf = make_file ~original_path ~ml_kind:Ml_kind.Intf in
          Some intf
      in
      Module.Source.make ~impl ~intf module_path
    in
    fun ~expander ~src_dir ~module_path ~for_ ->
      let src_dir = Path.build src_dir in
      let+ expanded =
        Modules_field_evaluator.expand_all_unchecked ~expander (Targets.modules ~for_)
      in
      let deps =
        Module_trie.Unchecked.fold
          expanded
          ~init:Path.Set.empty
          ~f:(fun (_, (_module_name, basename)) acc ->
            let original_path =
              let ext = Targets.extension ~for_ in
              Path.set_extension (Path.relative src_dir basename) ~ext
            in
            Path.Set.add acc original_path)
      in
      let targets =
        match for_ with
        | Ocamllex { loc; _ }
        | Ocamlyacc { loc; _ }
        | Menhir { Menhir_stanza.merge_into = None; loc; _ } ->
          Module_trie.Unchecked.map expanded ~f:(fun (_, (module_name, basename)) ->
            let module_path =
              Nonempty_list.(
                map (module_path @ [ module_name ]) ~f:Module_name.Unchecked.allow_invalid)
            in
            let original_path =
              let base_path = Path.relative src_dir basename in
              let ext = Targets.extension ~for_ in
              Path.set_extension base_path ~ext
            in
            loc, make_module ~module_path ~original_path ~for_)
        | Menhir { Menhir_stanza.merge_into = Some basename; loc; _ } ->
          let impl =
            let original_path =
              let ext =
                Dialect.extension Dialect.ocaml Ml_kind.Impl |> Option.value_exn
              in
              Path.set_extension (Path.relative src_dir basename) ~ext
            in
            Some (make_file ~original_path ~ml_kind:Ml_kind.Impl)
          in
          let intf =
            let original_path =
              let ext =
                Dialect.extension Dialect.ocaml Ml_kind.Intf |> Option.value_exn
              in
              Path.set_extension (Path.relative src_dir basename) ~ext
            in
            Some (make_file ~original_path ~ml_kind:Ml_kind.Intf)
          in
          let module_name = Module_name.of_string_allow_invalid (loc, basename) in
          let module_path = Nonempty_list.(module_path @ [ module_name ]) in
          let m =
            let module_path =
              Nonempty_list.map module_path ~f:Module_name.Unchecked.allow_invalid
            in
            Module.Source.make ~impl ~intf module_path
          in
          Module_trie.Unchecked.singleton module_path (loc, m)
      in
      let targets = Module_trie.Unchecked.check_exn targets in
      { deps; targets }
  ;;
end

let has_instances (lib : Buildable.t) =
  List.exists lib.libraries ~f:(function
    | Lib_dep.Instantiate _ -> true
    | Direct _ | Re_export _ | Select _ -> false)
;;

let make_lib_modules
      ~expander
      ~dir
      ~libs
      ~lookup_vlib
      ~(lib : Library.t)
      ~modules
      ~include_subdirs:(loc_include_subdirs, (include_subdirs : Include_subdirs.t))
      ~version
      ~for_
  =
  let open Resolve.Memo.O in
  let* kind, main_module_name, wrapped =
    match lib.implements with
    | None ->
      (* In the two following pattern matching, we can only get [From _] if
         [lib] is an implementation. Since we know that it is not one because of
         the above [match lib.implements with ...], we know that we can't get
         [From _]. That's why we have these [assert false]. *)
      let main_module_name =
        match Library.main_module_name lib with
        | This x -> x
        | From _ -> assert false
      in
      let wrapped =
        match lib.wrapped with
        | This x -> x
        | From _ -> assert false
      in
      let kind : Modules_field_evaluator.kind =
        match lib.kind, lib.virtual_modules with
        | Dune_file _, None -> Exe_or_normal_lib
        | Parameter, None -> Parameter
        | Virtual, Some virtual_modules -> Virtual { virtual_modules }
        | (Dune_file _ | Parameter), Some _ | Virtual, None -> assert false
      in
      Memo.return (Resolve.return (kind, main_module_name, wrapped))
    | Some _ ->
      assert (Option.is_none lib.virtual_modules);
      let open Memo.O in
      let* libs = libs in
      let* resolved =
        let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
        Lib.DB.find_lib_id_even_when_hidden libs (Local (Library.to_lib_id ~src_dir lib))
        (* can't happen because this library is defined using the current
           stanza *)
        >>| Option.value_exn
      in
      let open Resolve.Memo.O in
      (* This [Option.value_exn] is correct because the above [lib.implements]
         is [Some _] and this [lib] variable correspond to the same library. *)
      let* wrapped = Lib.wrapped resolved >>| Option.value_exn in
      let* main_module_name = Lib.main_module_name resolved in
      let+ kind =
        let+ impl =
          let* vlib = Lib.implements resolved |> Option.value_exn >>| Lib.info in
          virtual_modules ~lookup_vlib ~libs ~for_ vlib |> Resolve.Memo.lift_memo
        in
        Modules_field_evaluator.Implementation impl
      in
      kind, main_module_name, wrapped
  in
  let has_instances = has_instances lib.buildable in
  let open Memo.O in
  let* sources, modules =
    let { Buildable.loc = stanza_loc; modules = modules_settings; _ } = lib.buildable in
    Modules_field_evaluator.eval
      ~expander
      ~modules
      ~stanza_loc
      ~kind
      ~private_modules:
        (Option.value ~default:Ordered_set_lang.Unexpanded.standard lib.private_modules)
      ~src_dir:dir
      modules_settings
      ~version
      ~for_
  in
  let () =
    match lib.stdlib, include_subdirs with
    | Some stdlib, Include Qualified ->
      let main_message =
        Pp.text "a library with (stdlib ...) may not use (include_subdirs qualified)"
      in
      let annots =
        let main = User_message.make ~loc:loc_include_subdirs [ main_message ] in
        let related =
          [ User_message.make ~loc:stdlib.loc [ Pp.text "Already defined here" ] ]
        in
        User_message.Annots.singleton
          Compound_user_error.annot
          [ Compound_user_error.make ~main ~related ]
      in
      User_error.raise ~annots ~loc:loc_include_subdirs [ main_message ]
    | _, _ -> ()
  in
  let () =
    match lib.kind, Module_trie.as_singleton modules with
    | Parameter, None ->
      User_error.raise
        ~loc:lib.buildable.loc
        [ Pp.text "a library_parameter must declare exactly one module." ]
    | _ -> ()
  in
  let implements = Option.is_some lib.implements in
  let _loc, lib_name = lib.name in
  Resolve.Memo.return
    ( sources
    , Modules.lib
        ~stdlib:lib.stdlib
        ~implements
        ~has_instances
        ~lib_name
        ~obj_dir:dir
        ~modules
        ~main_module_name
        ~wrapped
        ~for_ )
;;

let module_path ~loc ~include_subdirs ~dir path_to_root =
  match include_subdirs with
  | Include_subdirs.No | Include Unqualified -> []
  | Include Qualified ->
    let loc =
      match loc with
      | Some loc -> loc
      | None -> Path.build dir |> Path.drop_optional_build_context |> Loc.in_dir
    in
    List.map path_to_root ~f:(fun m -> Module_name.of_string_allow_invalid (loc, m))
;;

module Generated_modules = struct
  type t =
    { modules : Module.Source.t Module_trie.Unchecked.t
    ; ocamllexes : Parser_generators.Stanzas.t Per_stanza.parser_gen_group list
    ; ocamlyaccs : Parser_generators.Stanzas.t Per_stanza.parser_gen_group list
    ; menhirs : Menhir_stanza.t Per_stanza.parser_gen_group list
    }

  let merge_two a b =
    (* Handle the corresponding opposite `Ml_kind.t` coming from one of the
       generated modules *)
    Module_trie.Unchecked.merge a b ~f:(fun _ m1 m2 ->
      match m1, m2 with
      | None, None -> None
      | Some m, None | None, Some m -> Some m
      | Some m1, Some m2 ->
        let { Ml_kind.Dict.impl; intf } =
          let files1 = Module.Source.files_by_ml_kind m1 in
          let files2 = Module.Source.files_by_ml_kind m2 in
          Ml_kind.Dict.of_func (fun ~ml_kind ->
            match Ml_kind.Dict.get files1 ml_kind, Ml_kind.Dict.get files2 ml_kind with
            | None, None -> None
            | Some m, None | (None | Some _), Some m -> Some m)
        in
        let m = Module.Source.make ~impl ~intf (Module.Source.path m1) in
        Some m)
  ;;

  let with_lib_select_deps =
    let parse_one_set
          ~dir
          (files : (Module_name.Unchecked.Path.t * (Loc.t * Module.File.t)) list)
      =
      match Module_name.Unchecked.Path.Map.of_list files with
      | Ok x -> x
      | Error (module_path, (loc, f1), (_, f2)) ->
        let src_dir = Path.Build.drop_build_context_exn dir in
        User_error.raise
          ~loc
          [ Pp.textf
              "Too many files for module %s in %s:"
              (Module_name.to_string
                 (Nonempty_list.last module_path |> Module_name.Unchecked.allow_invalid))
              (Path.Source.to_string_maybe_quoted src_dir)
          ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f1))
          ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f2))
          ]
    in
    fun ~dir ~dialects ~include_subdirs { modules; _ } libraries ->
      (* Manually add files generated by the (select ...) dependencies *)
      let impl_files, intf_files =
        List.filter_partition_map libraries ~f:(fun dep ->
          match (dep : Lib_dep.t) with
          | Re_export _ | Direct _ | Instantiate _ -> Skip
          | Select { loc; result_fn; choices = _ } ->
            let dst = Path.Build.append_local dir result_fn in
            (match
               Path.Local.descendant (Path.Build.local dst) ~of_:(Path.Build.local dir)
             with
             | None ->
               User_error.raise
                 ~loc
                 [ Pp.text
                     "`(select ..)' specifies targets in a directory that is not a \
                      descendant of the stanza directory."
                 ]
             | Some descendant ->
               let basename, ext = Path.Build.basename dst |> Filename.split_extension in
               (match Dialect.DB.find_by_extension dialects ext with
                | Some (dialect, ml_kind) ->
                  let file = Module.File.make dialect (Path.build dst) in
                  let module_path =
                    let base_path =
                      match include_subdirs with
                      | Include_subdirs.No | Include Unqualified -> []
                      | Include Qualified ->
                        module_path
                          ~loc:(Some loc)
                          ~include_subdirs
                          ~dir
                          (Path.Local.parent_exn descendant |> Path.Local.explode)
                    in
                    let module_name =
                      Module_name.of_string_allow_invalid (loc, basename)
                      |> Module_name.Unchecked.validate_exn
                    in
                    Nonempty_list.(base_path @ [ Module_name.unchecked module_name ])
                  in
                  (match ml_kind with
                   | Ml_kind.Impl -> Left (module_path, (loc, file))
                   | Intf -> Right (module_path, (loc, file)))
                | None -> Skip)))
      in
      let impls = parse_one_set ~dir impl_files in
      let intfs = parse_one_set ~dir intf_files in
      Module_name.Unchecked.Path.Map.merge impls intfs ~f:(fun path impl intf ->
        let path = Nonempty_list.map path ~f:Module_name.Unchecked.allow_invalid in
        let impl = Option.map ~f:snd impl in
        let intf = Option.map ~f:snd intf in
        Some (Module.Source.make path ~impl ~intf))
      |> Module_name.Unchecked.Path.Map.foldi
           ~init:Module_trie.Unchecked.empty
           ~f:(fun k m acc -> Module_trie.Unchecked.set acc k m)
      |> merge_two modules
  ;;

  let add_generated_modules =
    let filter_partition_map =
      let rev_filter_partition =
        let rec loop l (acc : t) =
          match l with
          | [] -> acc
          | x :: l ->
            let acc =
              List.fold_left x ~init:acc ~f:(fun acc x ->
                match x with
                | `Skip -> acc
                | `Ocamllex y -> { acc with ocamllexes = y :: acc.ocamllexes }
                | `Ocamlyacc y -> { acc with ocamlyaccs = y :: acc.ocamlyaccs }
                | `Menhir y -> { acc with menhirs = y :: acc.menhirs })
            in
            loop l acc
        in
        fun l ->
          loop
            l
            { modules = Module_trie.Unchecked.empty
            ; ocamllexes = []
            ; ocamlyaccs = []
            ; menhirs = []
            }
      in
      fun l ->
        let { modules; ocamllexes; ocamlyaccs; menhirs } = rev_filter_partition l in
        { modules
        ; ocamllexes = List.rev ocamllexes
        ; ocamlyaccs = List.rev ocamlyaccs
        ; menhirs = List.rev menhirs
        }
    in
    let merge_parser_targets ~ocamllexes ~ocamlyaccs ~menhirs modules =
      let parser_gen_modules =
        List.concat
          [ List.map ocamllexes ~f:(fun (x : _ Per_stanza.parser_gen_group) ->
              x.dep_info.targets)
          ; List.map ocamlyaccs ~f:(fun (x : _ Per_stanza.parser_gen_group) ->
              x.dep_info.targets)
          ; List.map menhirs ~f:(fun (x : _ Per_stanza.parser_gen_group) ->
              x.dep_info.targets)
          ]
        |> List.fold_left ~init:Module_trie.Unchecked.empty ~f:(fun acc targets ->
          Module_trie.fold targets ~init:acc ~f:(fun (_, m) acc ->
            let module_path =
              Nonempty_list.map (Module.Source.path m) ~f:Module_name.unchecked
            in
            Module_trie.Unchecked.set acc module_path m))
      in
      merge_two modules parser_gen_modules
    in
    fun ~expander ~include_subdirs ~dirs modules ->
      let+ ({ ocamllexes; ocamlyaccs; menhirs; _ } as generated_modules) =
        Memo.parallel_map
          (Nonempty_list.to_list dirs)
          ~f:(fun { Source_file_dir.dir; stanzas; path_to_root; _ } ->
            Memo.parallel_map stanzas ~f:(fun stanza ->
              let enabled_if =
                match Stanza.repr stanza with
                | Parser_generators.Stanzas.Ocamllex.T ocamllex -> ocamllex.enabled_if
                | Parser_generators.Stanzas.Ocamlyacc.T ocamlyacc -> ocamlyacc.enabled_if
                | Menhir_stanza.T menhir -> menhir.enabled_if
                | _ -> Blang.false_
              in
              Expander.eval_blang expander enabled_if
              >>= function
              | false -> Memo.return `Skip
              | true ->
                let module_path =
                  module_path ~loc:None ~include_subdirs ~dir path_to_root
                in
                (match Stanza.repr stanza with
                 | Parser_generators.Stanzas.Ocamllex.T ocamllex ->
                   let+ dep_info =
                     Parser_generators.expand_modules
                       ~expander
                       ~src_dir:dir
                       ~module_path
                       ~for_:(Ocamllex ocamllex)
                   in
                   `Ocamllex { Per_stanza.stanza = ocamllex; dep_info }
                 | Parser_generators.Stanzas.Ocamlyacc.T ocamlyacc ->
                   let+ dep_info =
                     Parser_generators.expand_modules
                       ~expander
                       ~src_dir:dir
                       ~module_path
                       ~for_:(Ocamlyacc ocamlyacc)
                   in
                   `Ocamlyacc { Per_stanza.stanza = ocamlyacc; dep_info }
                 | Menhir_stanza.T menhir ->
                   let+ dep_info =
                     Parser_generators.expand_modules
                       ~expander
                       ~src_dir:dir
                       ~module_path
                       ~for_:(Menhir menhir)
                   in
                   `Menhir { Per_stanza.stanza = menhir; dep_info }
                 | _ -> Memo.return `Skip)))
        >>| filter_partition_map
      in
      let modules = merge_parser_targets ~ocamllexes ~ocamlyaccs ~menhirs modules in
      { generated_modules with modules }
  ;;
end

let modules_of_stanzas =
  let filter_partition_map =
    let rev_filter_partition =
      let rec loop l (acc : Per_stanza.groups) =
        match l with
        | [] -> acc
        | x :: l ->
          let acc =
            List.fold_left x ~init:acc ~f:(fun (acc : Per_stanza.groups) x ->
              match x with
              | `Skip -> acc
              | `Library y -> { acc with libraries = y :: acc.libraries }
              | `Executables y -> { acc with executables = y :: acc.executables }
              | `Tests y -> { acc with tests = y :: acc.tests }
              | `Melange_emit y -> { acc with melange_emits = y :: acc.melange_emits })
          in
          loop l acc
      in
      fun l ->
        loop
          l
          { libraries = []
          ; executables = []
          ; tests = []
          ; melange_emits = []
          ; ocamllexes = []
          ; ocamlyaccs = []
          ; menhirs = []
          }
    in
    fun l ->
      let acc = rev_filter_partition l in
      { acc with
        Per_stanza.libraries = List.rev acc.libraries
      ; executables = List.rev acc.executables
      ; tests = List.rev acc.tests
      ; melange_emits = List.rev acc.melange_emits
      }
  in
  let make_executables
        ~dir
        ~expander
        ~(modules : Module.Source.t Module_trie.Unchecked.t)
        ~project
        exes
    =
    let obj_dir = Executables.obj_dir ~dir exes in
    let+ sources, modules =
      let { Buildable.loc = stanza_loc; modules = modules_settings; _ } =
        exes.buildable
      in
      Modules_field_evaluator.eval
        ~expander
        ~modules
        ~stanza_loc
        ~src_dir:dir
        ~kind:Modules_field_evaluator.Exe_or_normal_lib
        ~private_modules:Ordered_set_lang.Unexpanded.standard
        ~version:exes.dune_version
        modules_settings
        ~for_:Ocaml
    in
    let has_instances = has_instances exes.buildable in
    let modules =
      let obj_dir = Obj_dir.obj_dir obj_dir in
      if Dune_project.wrapped_executables project
      then Modules.make_wrapped ~obj_dir ~modules ~has_instances `Exe
      else Modules.exe_unwrapped modules ~obj_dir
    in
    `Executables { Per_stanza.stanza = exes; sources; modules; obj_dir; dir }
  in
  let make_tests ~dir ~expander ~modules ~project tests =
    let+ result = make_executables ~dir ~expander ~modules ~project tests.Tests.exes in
    match result with
    | `Executables group_part -> `Tests { group_part with stanza = tests }
  in
  fun (dirs : Source_file_dir.t Nonempty_list.t)
    ~expander
    ~project
    ~libs
    ~lookup_vlib
    ~(modules : Module.Source.t Module_trie.Unchecked.t)
    ~include_subdirs:(loc_include_subdirs, include_subdirs) ->
    let dialects = Dune_project.dialects project in
    let* ({ ocamllexes; ocamlyaccs; menhirs; _ } as modules) =
      Generated_modules.add_generated_modules ~expander ~include_subdirs ~dirs modules
    in
    Memo.parallel_map
      (Nonempty_list.to_list dirs)
      ~f:(fun { Source_file_dir.dir; stanzas; _ } ->
        Memo.parallel_map stanzas ~f:(fun stanza ->
          let enabled_if =
            match Stanza.repr stanza with
            | Library.T lib -> lib.enabled_if
            | Tests.T tests -> tests.exes.enabled_if
            | Executables.T exes -> exes.enabled_if
            | Melange_stanzas.Emit.T mel -> mel.enabled_if
            | _ -> Blang.false_
          in
          Expander.eval_blang expander enabled_if
          >>= function
          | false -> Memo.return `Skip
          | true ->
            (match Stanza.repr stanza with
             | Library.T lib ->
               (* jeremiedimino: this [Resolve.get] means that if the user writes an
                invalid [implements] field, we will get an error immediately even if
                the library is not built. We should change this to carry the
                [Or_exn.t] a bit longer. *)
               let+ sources, modules =
                 let lookup_vlib = lookup_vlib ~loc:lib.buildable.loc in
                 let modules =
                   Generated_modules.with_lib_select_deps
                     modules
                     ~dir
                     ~dialects
                     ~include_subdirs
                     lib.buildable.libraries
                 in
                 make_lib_modules
                   ~expander
                   ~dir
                   ~libs
                   ~lookup_vlib
                   ~modules
                   ~lib
                   ~include_subdirs:(loc_include_subdirs, include_subdirs)
                   ~version:lib.dune_version
                   ~for_:Ocaml
                 >>= Resolve.read_memo
               in
               let obj_dir = Library.obj_dir lib ~dir in
               `Library { Per_stanza.stanza = lib; sources; modules; dir; obj_dir }
             | Executables.T exes ->
               let modules =
                 Generated_modules.with_lib_select_deps
                   modules
                   ~dir
                   ~dialects
                   ~include_subdirs
                   exes.buildable.libraries
               in
               make_executables ~dir ~expander ~modules ~project exes
             | Tests.T tests ->
               let modules =
                 Generated_modules.with_lib_select_deps
                   modules
                   ~dir
                   ~dialects
                   ~include_subdirs
                   tests.exes.buildable.libraries
               in
               make_tests ~dir ~expander ~modules ~project tests
             | Melange_stanzas.Emit.T mel ->
               let obj_dir = Obj_dir.make_melange_emit ~dir ~name:mel.target in
               let+ sources, modules =
                 let modules =
                   Generated_modules.with_lib_select_deps
                     modules
                     ~dir
                     ~dialects
                     ~include_subdirs
                     mel.libraries
                 in
                 let version = Dune_project.dune_version project in
                 Modules_field_evaluator.eval
                   ~expander
                   ~modules
                   ~stanza_loc:mel.loc
                   ~kind:Modules_field_evaluator.Exe_or_normal_lib
                   ~version
                   ~private_modules:Ordered_set_lang.Unexpanded.standard
                   ~src_dir:dir
                   ~for_:Melange
                   mel.modules
               in
               let modules =
                 Modules.make_wrapped
                   ~obj_dir:(Obj_dir.obj_dir obj_dir)
                   ~modules
                   ~has_instances:false
                   `Melange
               in
               `Melange_emit { Per_stanza.stanza = mel; sources; modules; dir; obj_dir }
             | _ -> Memo.return `Skip)))
    >>| filter_partition_map
    >>| fun modules_of_stanzas ->
    { modules_of_stanzas with ocamllexes; ocamlyaccs; menhirs }
;;

let make
      ~expander
      ~libs
      ~project
      ~lib_config
      ~loc
      ~lookup_vlib
      ~include_subdirs:(loc_include_subdirs, (include_subdirs : Include_subdirs.t))
      (dirs : Source_file_dir.t Nonempty_list.t)
  =
  let+ modules_of_stanzas =
    let modules =
      let dirs = Nonempty_list.to_list dirs in
      let dialects = Dune_project.dialects project in
      match include_subdirs with
      | Include Qualified ->
        List.fold_left
          dirs
          ~init:Module_trie.Unchecked.empty
          ~f:(fun acc { Source_file_dir.dir; files; path_to_root; _ } ->
            match
              let path = module_path ~loc:None ~include_subdirs ~dir path_to_root in
              let modules = modules_of_files ~dialects ~dir ~files ~path in
              Module_trie.Unchecked.set_map acc path modules
            with
            | Ok s -> s
            | Error module_ ->
              let module_ =
                match module_ with
                | Leaf m ->
                  Module.Source.files m
                  |> List.hd
                  |> Module.File.path
                  |> Path.drop_optional_build_context
                  |> Path.to_string_maybe_quoted
                | Map _ ->
                  (* it's not possible to define the same group twice because
                     there can be at most one directory *)
                  assert false
              in
              let group =
                (dir
                 |> Path.Build.drop_build_context_exn
                 |> Path.Source.to_string_maybe_quoted)
                ^ "/"
              in
              User_error.raise
                ~loc
                [ Pp.text
                    "The following module and module group cannot co-exist in the same \
                     executable or library because they correspond to the same module \
                     path"
                ; Pp.textf "- module %s" module_
                ; Pp.textf "- module group %s" group
                ])
      | No | Include Unqualified ->
        let modules =
          List.fold_left
            dirs
            ~init:Module_name.Unchecked.Map.empty
            ~f:(fun acc { Source_file_dir.dir; files; _ } ->
              let modules = modules_of_files ~dialects ~dir ~files ~path:[] in
              Module_name.Unchecked.Map.union acc modules ~f:(fun name x y ->
                User_error.raise
                  ~loc
                  [ Pp.textf
                      "Module %S appears in several directories:"
                      (Module_name.to_string (Module_name.Unchecked.allow_invalid name))
                  ; Pp.textf
                      "- %s"
                      (Path.to_string_maybe_quoted (Module.Source.src_dir x))
                  ; Pp.textf
                      "- %s"
                      (Path.to_string_maybe_quoted (Module.Source.src_dir y))
                  ; Pp.text "This is not allowed, please rename one of them."
                  ]))
        in
        Module_trie.Unchecked.of_map modules
    in
    modules_of_stanzas
      dirs
      ~expander
      ~project
      ~libs
      ~lookup_vlib
      ~modules
      ~include_subdirs:(loc_include_subdirs, include_subdirs)
  in
  let modules = Per_stanza.make modules_of_stanzas in
  let artifacts =
    Memo.lazy_ (fun () ->
      let libs =
        List.map modules_of_stanzas.libraries ~f:(fun (part : _ Per_stanza.group_part) ->
          part.stanza, part.modules, part.obj_dir)
      in
      let exes =
        let modules_and_obj_dir { Per_stanza.modules; obj_dir; _ } = modules, obj_dir in
        List.concat
          [ List.map modules_of_stanzas.executables ~f:modules_and_obj_dir
          ; List.map modules_of_stanzas.tests ~f:modules_and_obj_dir
          ]
      in
      let { Source_file_dir.dir; _ } = Nonempty_list.hd dirs in
      Artifacts_obj.make
        ~dir
        ~expander:(Expander.to_expander0 expander)
        ~lib_config
        ~libs
        ~exes)
  in
  { modules; artifacts; include_subdirs }
;;
