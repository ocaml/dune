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

module Per_stanza = struct
  type component = Origin.t * Modules.t * Path.Build.t Obj_dir.t

  type t =
    { libraries : component Lib_id.Local.Map.t
    ; executables : component String.Map.t
    ; melange_emits : component String.Map.t
    ; ocamllexes : Module.Source.t Module_trie.t Loc.Map.t
    ; ocamlyaccs : Module.Source.t Module_trie.t Loc.Map.t
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
    ; sources : Module.Source.t Module_trie.t
    }

  type groups =
    { libraries : Library.t group_part list
    ; executables : Executables.t group_part list
    ; tests : Tests.t group_part list
    ; melange_emits : Melange_stanzas.Emit.t group_part list
    ; ocamllexes : Parser_generators.t parser_gen_group list
    ; ocamlyaccs : Parser_generators.t parser_gen_group list
    }

  let make
        { libraries = libs
        ; executables = exes
        ; tests
        ; melange_emits = emits
        ; ocamllexes
        ; ocamlyaccs
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
      Loc.Map.of_list_map_exn ocamllexes ~f:(fun { stanza; sources } ->
        stanza.loc, sources)
    in
    let ocamlyaccs =
      Loc.Map.of_list_map_exn ocamlyaccs ~f:(fun { stanza; sources } ->
        stanza.loc, sources)
    in
    let rev_map =
      let by_path (origin : Origin.t * Path.Build.t) trie =
        Module_trie.fold trie ~init:[] ~f:(fun (_loc, m) acc ->
          (Module.Source.path m, origin) :: acc)
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

let modules_of_files_gen =
  let parse_one_set (files : (string * Module.File.t) list) ~dir =
    match Filename.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let loc = Loc.in_dir dir in
      let name = Module_name.of_string_allow_invalid (loc, name) in
      let src_dir = Path.drop_build_context_exn dir in
      User_error.raise
        ~loc
        [ Pp.textf
            "Too many files for module %s in %s:"
            (Module_name.to_string name)
            (Path.Source.to_string_maybe_quoted src_dir)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f1))
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f2))
        ]
  in
  fun ~dir (impl_files, intf_files) ->
    let impls = parse_one_set impl_files ~dir in
    let intfs = parse_one_set intf_files ~dir in
    let loc = Loc.in_dir dir in
    Filename.Map.merge impls intfs ~f:(fun name impl intf ->
      let module_path =
        (* later replaced *)
        Nonempty_list.[ Module_name.of_string_allow_invalid (loc, name) ]
      in
      Some (Module.Source.make module_path ~impl ~intf))
;;

let modules_of_files ~dialects ~dir ~files =
  let dir = Path.build dir in
  let make_module dialect name fn =
    name, Module.File.make dialect (Path.relative dir fn)
  in
  Filename.Set.to_list files
  |> List.filter_partition_map ~f:(fun fn ->
    (* we aren't using Filename.extension because we want to handle
       filenames such as foo.cppo.ml *)
    match String.lsplit2 fn ~on:'.' with
    | None -> Skip
    | Some (s, ext) ->
      (match Dialect.DB.find_by_extension dialects ("." ^ ext) with
       | None -> Skip
       | Some (dialect, ml_kind) ->
         let module_ = make_module dialect s fn in
         (match ml_kind with
          | Impl -> Left module_
          | Intf -> Right module_)))
  |> modules_of_files_gen ~dir
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

let virtual_modules ~lookup_vlib ~libs vlib =
  let+ modules =
    match Lib_info.modules vlib with
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

  type for_ =
    | Ocamllex of Loc.t
    | Ocamlyacc of Loc.t

  let source_modules t ~for_ =
    match for_ with
    | Ocamllex loc -> Loc.Map.find t.modules.ocamllexes loc
    | Ocamlyacc loc -> Loc.Map.find t.modules.ocamlyaccs loc
  ;;

  type t =
    | Ocamllex
    | Parser

  let classify = function
    | "mll" -> Some Ocamllex
    | "mly" -> Some Parser
    | _ -> None
  ;;

  let modules_of_files =
    let make_module module_name ~original_path ~ml_kind =
      let ext = Dialect.extension Dialect.ocaml ml_kind |> Option.value_exn in
      let path = Path.set_extension original_path ~ext in
      module_name, Module.File.make ~original_path Dialect.ocaml path
    in
    fun ~dir ~files ->
      let dir = Path.build dir in
      Filename.Set.to_list files
      |> List.concat_map ~f:(fun fn ->
        (* we aren't using Filename.extension because we want to handle
           filenames such as foo.cppo.ml *)
        match String.lsplit2 fn ~on:'.' with
        | None -> [ List.Skip ]
        | Some (s, ext) ->
          (match classify ext with
           | Some kind ->
             let original_path = Path.relative dir fn in
             (match kind with
              | Ocamllex ->
                let module_ = make_module s ~original_path ~ml_kind:Ml_kind.Impl in
                [ Left module_ ]
              | Parser ->
                let impl = make_module s ~original_path ~ml_kind:Ml_kind.Impl
                and intf = make_module s ~original_path ~ml_kind:Ml_kind.Intf in
                [ Left impl; Right intf ])
           | None -> [ Skip ]))
      |> List.filter_partition_map ~f:Fun.id
      |> modules_of_files_gen ~dir
  ;;

  let eval_source_modules
        ~src_dir
        ~project
        ~expander
        ~all_modules:modules
        ~module_path
        ~loc
        modules_osl
    =
    let version = Dune_project.dune_version project in
    Modules_field_evaluator.eval
      ~expander
      ~modules
      ~module_path
      ~stanza_loc:loc
      ~kind:Exe_or_normal_lib
      ~version
      ~private_modules:Ordered_set_lang.Unexpanded.standard
      ~src_dir
      (Parser_generators.modules_settings modules_osl)
    >>| fst
    >>| Module_trie.map ~f:snd
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
          virtual_modules ~lookup_vlib ~libs vlib |> Resolve.Memo.lift_memo
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
      ~module_path:None
      ~stanza_loc
      ~kind
      ~private_modules:
        (Option.value ~default:Ordered_set_lang.Unexpanded.standard lib.private_modules)
      ~src_dir:dir
      modules_settings
      ~version
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
        ~wrapped )
;;

let modules_of_stanzas =
  let filter_partition_map =
    let rev_filter_partition =
      let rec loop l (acc : Per_stanza.groups) =
        match l with
        | [] -> acc
        | x :: l ->
          (match x with
           | `Skip -> loop l acc
           | `Library y -> loop l { acc with libraries = y :: acc.libraries }
           | `Executables y -> loop l { acc with executables = y :: acc.executables }
           | `Tests y -> loop l { acc with tests = y :: acc.tests }
           | `Melange_emit y -> loop l { acc with melange_emits = y :: acc.melange_emits }
           | `Ocamllex y -> loop l { acc with ocamllexes = y :: acc.ocamllexes }
           | `Ocamlyacc y -> loop l { acc with ocamlyaccs = y :: acc.ocamlyaccs })
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
          }
    in
    fun l ->
      let { Per_stanza.libraries
          ; executables
          ; tests
          ; melange_emits
          ; ocamllexes
          ; ocamlyaccs
          }
        =
        rev_filter_partition l
      in
      { Per_stanza.libraries = List.rev libraries
      ; executables = List.rev executables
      ; tests = List.rev tests
      ; melange_emits = List.rev melange_emits
      ; ocamllexes = List.rev ocamllexes
      ; ocamlyaccs = List.rev ocamlyaccs
      }
  in
  let make_executables
        ~dir
        ~expander
        ~(modules : Module.Source.t Module_trie.t)
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
        ~module_path:None
        ~stanza_loc
        ~src_dir:dir
        ~kind:Modules_field_evaluator.Exe_or_normal_lib
        ~private_modules:Ordered_set_lang.Unexpanded.standard
        ~version:exes.dune_version
        modules_settings
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
  let merge_parser_sources modules ~ocamllexes ~ocamlyaccs =
    let parser_gen_modules =
      List.concat
        [ List.map ocamllexes ~f:(fun (x : _ Per_stanza.parser_gen_group) -> x.sources)
        ; List.map ocamlyaccs ~f:(fun (x : _ Per_stanza.parser_gen_group) -> x.sources)
        ]
      |> List.fold_left ~init:Module_trie.empty ~f:(fun acc sources ->
        Module_trie.fold sources ~init:acc ~f:(fun m acc ->
          Module_trie.set acc (Module.Source.path m) m))
    in
    Module_trie.merge modules parser_gen_modules ~f:(fun _ m1 m2 ->
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
            | Some m, None | _, Some m -> Some m)
        in
        let m = Module.Source.make ~impl ~intf (Module.Source.path m1) in
        Some m)
  in
  fun (dirs : Source_file_dir.t Nonempty_list.t)
    ~expander
    ~project
    ~libs
    ~lookup_vlib
    ~(modules : Module.Source.t Module_trie.t Memo.Lazy.t)
    ~(parser_gen_modules : Module.Source.t Module_trie.t Memo.Lazy.t)
    ~include_subdirs ->
    let { Source_file_dir.dir; _ } = Nonempty_list.hd dirs in
    let dirs =
      match include_subdirs with
      | _, (Include_subdirs.No | Include Unqualified) ->
        Nonempty_list.to_list_map dirs ~f:(fun (dune_file : Source_file_dir.t) ->
          lazy None, dune_file.stanzas)
      | _, Include Qualified ->
        Nonempty_list.to_list_map dirs ~f:(fun (dune_file : Source_file_dir.t) ->
          ( lazy
              (let path =
                 let dir = Path.build dir in
                 let loc = Loc.in_dir (Path.drop_optional_build_context dir) in
                 List.map dune_file.path_to_root ~f:(fun m ->
                   Module_name.parse_string_exn (loc, m))
               in
               Some path)
          , dune_file.stanzas ))
    in
    let* ocamllexes, ocamlyaccs =
      Memo.parallel_map dirs ~f:(fun (module_path, stanzas) ->
        Memo.parallel_map stanzas ~f:(fun stanza ->
          let enabled_if =
            match Stanza.repr stanza with
            | Parser_generators.Stanzas.Ocamllex.T ocamllex -> ocamllex.enabled_if
            | Parser_generators.Stanzas.Ocamlyacc.T ocamlyacc -> ocamlyacc.enabled_if
            | _ -> Blang.false_
          in
          Expander.eval_blang expander enabled_if
          >>= function
          | false -> Memo.return List.Skip
          | true ->
            (match Stanza.repr stanza with
             | Parser_generators.Stanzas.Ocamllex.T ocamllex ->
               let+ sources =
                 let* all_modules = Memo.Lazy.force parser_gen_modules in
                 let module_path = Lazy.force module_path in
                 Parser_generators.eval_source_modules
                   ~src_dir:dir
                   ~project
                   ~expander
                   ~all_modules
                   ~module_path
                   ~loc:ocamllex.loc
                   ocamllex.modules
               in
               List.Left { Per_stanza.stanza = ocamllex; sources }
             | Parser_generators.Stanzas.Ocamlyacc.T ocamlyacc ->
               let+ sources =
                 let* all_modules = Memo.Lazy.force parser_gen_modules in
                 let module_path = Lazy.force module_path in
                 Parser_generators.eval_source_modules
                   ~src_dir:dir
                   ~project
                   ~expander
                   ~all_modules
                   ~module_path
                   ~loc:ocamlyacc.loc
                   ocamlyacc.modules
               in
               List.Right { Per_stanza.stanza = ocamlyacc; sources }
             | _ -> Memo.return List.Skip)))
      >>| List.concat
      >>| List.filter_partition_map ~f:Fun.id
    in
    let* modules =
      Memo.Lazy.force modules >>| merge_parser_sources ~ocamllexes ~ocamlyaccs
    in
    Memo.parallel_map dirs ~f:(fun (_, stanzas) ->
      Memo.parallel_map stanzas ~f:(fun stanza ->
        let enabled_if =
          match Stanza.repr stanza with
          | Library.T lib -> lib.enabled_if
          | Tests.T tests -> tests.exes.enabled_if
          | Executables.T exes -> exes.enabled_if
          | Melange_stanzas.Emit.T mel -> mel.enabled_if
          | _ -> Blang.true_
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
               make_lib_modules
                 ~expander
                 ~dir
                 ~libs
                 ~lookup_vlib
                 ~modules
                 ~lib
                 ~include_subdirs
                 ~version:lib.dune_version
               >>= Resolve.read_memo
             in
             let obj_dir = Library.obj_dir lib ~dir in
             `Library { Per_stanza.stanza = lib; sources; modules; dir; obj_dir }
           | Executables.T exes -> make_executables ~dir ~expander ~modules ~project exes
           | Tests.T tests -> make_tests ~dir ~expander ~modules ~project tests
           | Melange_stanzas.Emit.T mel ->
             let obj_dir = Obj_dir.make_melange_emit ~dir ~name:mel.target in
             let+ sources, modules =
               let version = Dune_project.dune_version project in
               Modules_field_evaluator.eval
                 ~expander
                 ~modules
                 ~module_path:None
                 ~stanza_loc:mel.loc
                 ~kind:Modules_field_evaluator.Exe_or_normal_lib
                 ~version
                 ~private_modules:Ordered_set_lang.Unexpanded.standard
                 ~src_dir:dir
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
    >>| List.concat
    >>| filter_partition_map
    >>| fun modules_of_stanzas -> { modules_of_stanzas with ocamllexes; ocamlyaccs }
;;

let module_trie_by_module_name modules =
  Memo.lazy_ (fun () ->
    Module_trie.By_dir.foldi modules ~init:Module_trie.empty ~f:(fun module_key m acc ->
      let path =
        let file = Module.Source.files m |> List.hd in
        let dir = Module.File.path file |> Path.parent_exn in
        let loc = Path.drop_optional_build_context dir |> Loc.in_dir in
        let (module_name :: dirs) = Nonempty_list.rev module_key in
        let module_name = Module_name.of_string_allow_invalid (loc, module_name) in
        let dirs = List.map dirs ~f:(fun s -> Module_name.parse_string_exn (loc, s)) in
        Nonempty_list.(rev (module_name :: dirs))
      in
      let m =
        let { Ml_kind.Dict.impl; intf } = Module.Source.files_by_ml_kind m in
        Module.Source.make ~impl ~intf path
      in
      Module_trie.set acc path m)
    |> Memo.return)
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
    let modules, parser_gen_modules =
      let dirs = Nonempty_list.to_list dirs in
      let dialects = Dune_project.dialects project in
      match include_subdirs with
      | Include Qualified ->
        let set_modules acc path modules =
          match path with
          | [] -> Ok (Module_trie.By_dir.of_map modules)
          | p :: path -> Module_trie.By_dir.set_map acc (p :: path) modules
        in
        List.fold_left
          dirs
          ~init:(Module_trie.By_dir.empty, Module_trie.By_dir.empty)
          ~f:
            (fun
              (acc, acc_parsing)
              { Source_file_dir.dir; path_to_root; files; source_dir = _; stanzas = _ }
            ->
            match
              ( modules_of_files ~dialects ~dir ~files |> set_modules acc path_to_root
              , Parser_generators.modules_of_files ~dir ~files
                |> set_modules acc_parsing path_to_root )
            with
            | Ok s, Ok s' -> s, s'
            | Error module_, _ | _, Error module_ ->
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
        let modules, parser_gen_modules =
          List.fold_left
            dirs
            ~init:(Filename.Map.empty, Filename.Map.empty)
            ~f:
              (fun
                (acc, acc_parsing)
                { Source_file_dir.dir
                ; files
                ; path_to_root = _
                ; source_dir = _
                ; stanzas = _
                }
              ->
              let modules = modules_of_files ~dialects ~dir ~files in
              let parser_gen_modules = Parser_generators.modules_of_files ~dir ~files in
              let union_exn name x y =
                User_error.raise
                  ~loc
                  [ Pp.textf
                      "Module %S appears in several directories:"
                      (Module_name.of_string_allow_invalid (Loc.none, name)
                       |> Module_name.to_string)
                  ; Pp.textf
                      "- %s"
                      (Path.to_string_maybe_quoted (Module.Source.src_dir x))
                  ; Pp.textf
                      "- %s"
                      (Path.to_string_maybe_quoted (Module.Source.src_dir y))
                  ; Pp.text "This is not allowed, please rename one of them."
                  ]
              in
              ( Filename.Map.union acc modules ~f:union_exn
              , Filename.Map.union acc_parsing parser_gen_modules ~f:union_exn ))
        in
        Module_trie.By_dir.of_map modules, Module_trie.By_dir.of_map parser_gen_modules
    in
    let modules, parser_gen_modules =
      module_trie_by_module_name modules, module_trie_by_module_name parser_gen_modules
    in
    modules_of_stanzas
      dirs
      ~expander
      ~project
      ~libs
      ~lookup_vlib
      ~modules
      ~parser_gen_modules
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
