open Import
open Memo.O
module Modules_group = Modules

module Origin = struct
  type t =
    | Library of Library.t
    | Executables of Executables.t
    | Melange of Melange_stanzas.Emit.t

  let loc = function
    | Library l -> l.buildable.loc
    | Executables e -> e.buildable.loc
    | Melange mel -> mel.loc
  ;;

  let preprocess = function
    | Library l -> l.buildable.preprocess
    | Executables e -> e.buildable.preprocess
    | Melange mel -> mel.preprocess
  ;;

  let to_dyn = function
    | Library _ -> Dyn.variant "Library" [ Dyn.Opaque ]
    | Executables _ -> Dyn.variant "Executables" [ Dyn.Opaque ]
    | Melange _ -> Dyn.variant "Melange" [ Dyn.Opaque ]
  ;;
end

module Modules = struct
  type component = Origin.t * Modules_group.t * Path.Build.t Obj_dir.t

  type t =
    { libraries : component Lib_id.Local.Map.t
    ; executables : component String.Map.t
    ; melange_emits : component String.Map.t
    ; (* Map from modules to the origin they are part of *)
      rev_map : (Origin.t * Path.Build.t) list Module_name.Path.Map.t
    ; libraries_by_obj_dir : Lib_id.Local.t list Path.Build.Map.t
    }

  let empty =
    { libraries = Lib_id.Local.Map.empty
    ; executables = String.Map.empty
    ; melange_emits = String.Map.empty
    ; rev_map = Module_name.Path.Map.empty
    ; libraries_by_obj_dir = Path.Build.Map.empty
    }
  ;;

  type 'stanza group_part =
    { stanza : 'stanza
    ; sources : (Loc.t * Module.Source.t) Module_trie.t
    ; modules : Modules_group.t
    ; dir : Path.Build.t
    ; obj_dir : Path.Build.t Obj_dir.t
    }

  type groups =
    { libraries : Library.t group_part list
    ; executables : Executables.t group_part list
    ; melange_emits : Melange_stanzas.Emit.t group_part list
    }

  let make { libraries = libs; executables = exes; melange_emits = emits } =
    let libraries, libraries_by_obj_dir =
      List.fold_left
        libs
        ~init:(Lib_id.Local.Map.empty, Path.Build.Map.empty)
        ~f:(fun (by_id, by_obj_dir) part ->
          let origin : Origin.t = Library part.stanza in
          let lib_id =
            let src_dir =
              Path.drop_optional_build_context_src_exn (Path.build part.dir)
            in
            Library.to_lib_id ~src_dir part.stanza
          in
          let by_id =
            Lib_id.Local.Map.add_exn by_id lib_id (origin, part.modules, part.obj_dir)
          and by_obj_dir =
            Path.Build.Map.update by_obj_dir (Obj_dir.obj_dir part.obj_dir) ~f:(function
              | None -> Some [ lib_id ]
              | Some lib_ids -> Some (lib_id :: lib_ids))
          in
          by_id, by_obj_dir)
    in
    let executables =
      match
        String.Map.of_list_map exes ~f:(fun (part : Executables.t group_part) ->
          let first_exe = snd (Nonempty_list.hd part.stanza.names) in
          let origin : Origin.t = Executables part.stanza in
          first_exe, (origin, part.modules, part.obj_dir))
      with
      | Ok x -> x
      | Error (name, _, part) ->
        User_error.raise
          ~loc:part.stanza.buildable.loc
          [ Pp.textf "Executable %S appears for the second time in this directory" name ]
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
    let rev_map =
      let modules =
        let by_path (origin : Origin.t * Path.Build.t) trie =
          Module_trie.fold trie ~init:[] ~f:(fun (_loc, m) acc ->
            (Module.Source.path m, origin) :: acc)
        in
        List.concat
          [ List.concat_map libs ~f:(fun part ->
              by_path (Library part.stanza, part.dir) part.sources)
          ; List.concat_map exes ~f:(fun part ->
              by_path (Executables part.stanza, part.dir) part.sources)
          ; List.concat_map emits ~f:(fun part ->
              by_path (Melange part.stanza, part.dir) part.sources)
          ]
      in
      List.fold_left
        modules
        ~init:Module_name.Path.Map.empty
        ~f:(fun module_name_map (module_name, origin) ->
          Module_name.Path.Map.update module_name_map module_name ~f:(function
            | None -> Some [ origin ]
            | Some origins -> Some (origin :: origins)))
    in
    { libraries; executables; melange_emits; rev_map; libraries_by_obj_dir }
  ;;
end

type t =
  { modules : Modules.t
  ; artifacts : Artifacts_obj.t Memo.Lazy.t
  ; include_subdirs : Include_subdirs.t
  }

let include_subdirs t = t.include_subdirs

let empty =
  { modules = Modules.empty
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
      (* we aren't using Filename.extension because we want to handle
         filenames such as foo.cppo.ml *)
      match String.lsplit2 fn ~on:'.' with
      | None -> Skip
      | Some (s, ext) ->
        (match Dialect.DB.find_by_extension dialects ("." ^ ext) with
         | None -> Skip
         | Some (dialect, ml_kind) ->
           let name = Module_name.of_string_allow_invalid (loc, s) in
           let module_ = make_module dialect name fn in
           (match ml_kind with
            | Impl -> Left module_
            | Intf -> Right module_)))
  in
  let parse_one_set (files : (Module_name.t * Module.File.t) list) =
    match Module_name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      User_error.raise
        [ Pp.textf
            "Too many files for module %s in %s:"
            (Module_name.to_string name)
            (Path.Source.to_string_maybe_quoted src_dir)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f1))
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f2))
        ]
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module_name.Map.merge impls intfs ~f:(fun name impl intf ->
    Some (Module.Source.make (path @ [ name ]) ?impl ?intf))
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
    let* origins =
      Memo.List.filter_map origins ~f:(fun (origin, dir) ->
        match origin with
        | Executables _ | Melange _ -> Memo.return (Some origin)
        | Library lib ->
          let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
          Lib.DB.available_by_lib_id libs (Local (Library.to_lib_id ~src_dir lib))
          >>| (function
           | false -> None
           | true -> Some origin))
    in
    (match origins with
     | [] -> Memo.return None
     | [ origin ] -> Memo.return (Some origin)
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
      Modules_group.fold_user_written modules ~init:[] ~f:(fun m acc ->
        Module.path m :: acc)
      |> Memo.List.iter ~f:(fun module_path ->
        let+ (_origin : Origin.t option) = find_origin t ~libs module_path in
        ())
    in
    (match
       Path.Build.Map.find_exn t.modules.libraries_by_obj_dir (Obj_dir.obj_dir obj_dir)
     with
     | [] | [ _ ] -> Memo.return (modules, obj_dir)
     | lib_ids ->
       let+ lib_ids =
         Memo.List.filter lib_ids ~f:(fun lib_id ->
           Lib.DB.available_by_lib_id libs (Local lib_id))
       in
       (match lib_ids with
        | [] | [ _ ] -> modules, obj_dir
        | lib_ids ->
          let lib_id =
            let lib_ids =
              List.sort lib_ids ~compare:(fun a b ->
                Loc.compare (Lib_id.Local.loc a) (Lib_id.Local.loc b))
            in
            (* Get the 2nd loc *)
            lib_ids |> List.tl |> List.hd
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
  let info = Lib.info vlib in
  let+ modules =
    match Option.value_exn (Lib_info.virtual_ info) with
    | External modules -> Memo.return modules
    | Local ->
      let src_dir = Lib_info.src_dir info |> Path.as_in_build_dir_exn in
      let* t = lookup_vlib ~dir:src_dir in
      modules t ~libs ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
  in
  let existing_virtual_modules = Modules_group.virtual_module_names modules in
  let allow_new_public_modules =
    Modules_group.wrapped modules |> Wrapped.to_bool |> not
  in
  { Modules_field_evaluator.Implementation.existing_virtual_modules
  ; allow_new_public_modules
  }
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
        match lib.virtual_modules with
        | None -> Exe_or_normal_lib
        | Some virtual_modules -> Virtual { virtual_modules }
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
      let+ impl =
        let* vlib = Lib.implements resolved |> Option.value_exn in
        virtual_modules ~lookup_vlib ~libs vlib |> Resolve.Memo.lift_memo
      in
      let kind : Modules_field_evaluator.kind = Implementation impl in
      kind, main_module_name, wrapped
  in
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
  let implements = Option.is_some lib.implements in
  let _loc, lib_name = lib.name in
  Resolve.Memo.return
    ( sources
    , Modules_group.lib
        ~stdlib:lib.stdlib
        ~implements
        ~lib_name
        ~obj_dir:dir
        ~modules
        ~main_module_name
        ~wrapped )
;;

let modules_of_stanzas =
  let filter_partition_map =
    let rev_filter_partition =
      let rec loop l (acc : Modules.groups) =
        match l with
        | [] -> acc
        | x :: l ->
          (match x with
           | `Skip -> loop l acc
           | `Library y -> loop l { acc with libraries = y :: acc.libraries }
           | `Executables y -> loop l { acc with executables = y :: acc.executables }
           | `Melange_emit y -> loop l { acc with melange_emits = y :: acc.melange_emits })
      in
      fun l -> loop l { libraries = []; executables = []; melange_emits = [] }
    in
    fun l ->
      let { Modules.libraries; executables; melange_emits } = rev_filter_partition l in
      { Modules.libraries = List.rev libraries
      ; executables = List.rev executables
      ; melange_emits = List.rev melange_emits
      }
  in
  let make_executables ~dir ~expander ~modules ~project exes =
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
    in
    let modules =
      let obj_dir = Obj_dir.obj_dir obj_dir in
      if Dune_project.wrapped_executables project
      then Modules_group.make_wrapped ~obj_dir ~modules `Exe
      else Modules_group.exe_unwrapped modules ~obj_dir
    in
    `Executables { Modules.stanza = exes; sources; modules; obj_dir; dir }
  in
  fun stanzas ~expander ~project ~dir ~libs ~lookup_vlib ~modules ~include_subdirs ->
    Memo.parallel_map stanzas ~f:(fun stanza ->
      let enabled_if =
        match Stanza.repr stanza with
        | Library.T lib -> lib.enabled_if
        | Tests.T exes -> exes.build_if
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
           `Library { Modules.stanza = lib; sources; modules; dir; obj_dir }
         | Executables.T exes -> make_executables ~dir ~expander ~modules ~project exes
         | Tests.T { exes; _ } -> make_executables ~dir ~expander ~modules ~project exes
         | Melange_stanzas.Emit.T mel ->
           let obj_dir = Obj_dir.make_melange_emit ~dir ~name:mel.target in
           let+ sources, modules =
             Modules_field_evaluator.eval
               ~expander
               ~modules
               ~stanza_loc:mel.loc
               ~kind:Modules_field_evaluator.Exe_or_normal_lib
               ~version:mel.dune_version
               ~private_modules:Ordered_set_lang.Unexpanded.standard
               ~src_dir:dir
               mel.modules
           in
           let modules =
             Modules_group.make_wrapped
               ~obj_dir:(Obj_dir.obj_dir obj_dir)
               ~modules
               `Melange
           in
           `Melange_emit { Modules.stanza = mel; sources; modules; dir; obj_dir }
         | _ -> Memo.return `Skip))
    >>| filter_partition_map
;;

let make
  dune_file
  ~expander
  ~dir
  ~libs
  ~project
  ~lib_config
  ~loc
  ~lookup_vlib
  ~include_subdirs:(loc_include_subdirs, (include_subdirs : Include_subdirs.t))
  ~dirs
  =
  let+ modules_of_stanzas =
    let modules =
      let dialects = Dune_project.dialects project in
      match include_subdirs with
      | Include Qualified ->
        List.fold_left
          dirs
          ~init:Module_trie.empty
          ~f:(fun acc { Source_file_dir.dir; path_to_root; files } ->
            let path =
              List.map path_to_root ~f:(fun m ->
                Module_name.parse_string_exn
                  (Loc.in_dir (Path.drop_optional_build_context (Path.build dir)), m))
            in
            let modules = modules_of_files ~dialects ~dir ~files ~path in
            match Module_trie.set_map acc path modules with
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
        List.fold_left
          dirs
          ~init:Module_name.Map.empty
          ~f:(fun acc { Source_file_dir.dir; files; path_to_root = _ } ->
            let modules = modules_of_files ~dialects ~dir ~files ~path:[] in
            Module_name.Map.union acc modules ~f:(fun name x y ->
              User_error.raise
                ~loc
                [ Pp.textf
                    "Module %S appears in several directories:"
                    (Module_name.to_string name)
                ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.Source.src_dir x))
                ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.Source.src_dir y))
                ; Pp.text "This is not allowed, please rename one of them."
                ]))
        |> Module_trie.of_map
    in
    modules_of_stanzas
      dune_file
      ~expander
      ~project
      ~dir
      ~libs
      ~lookup_vlib
      ~modules
      ~include_subdirs:(loc_include_subdirs, include_subdirs)
  in
  let modules = Modules.make modules_of_stanzas in
  let artifacts =
    Memo.lazy_ (fun () ->
      let libs =
        List.map modules_of_stanzas.libraries ~f:(fun (part : _ Modules.group_part) ->
          part.stanza, part.modules, part.obj_dir)
      in
      let exes =
        List.map modules_of_stanzas.executables ~f:(fun (part : _ Modules.group_part) ->
          part.modules, part.obj_dir)
      in
      Artifacts_obj.make
        ~dir
        ~expander:(Expander.to_expander0 expander)
        ~lib_config
        ~libs
        ~exes)
  in
  { modules; artifacts; include_subdirs }
;;
