open Import
open Dune_file
open Memo.O
module Modules_group = Modules

module Modules = struct
  type t =
    { libraries : (Modules.t * Path.Build.t Obj_dir.t) Lib_name.Map.t
    ; executables : (Modules.t * Path.Build.t Obj_dir.t) String.Map.t
    ; (* Map from modules to the buildable they are part of *)
      rev_map :
        [ `Library of Dune_file.Library.t
        | `Executables of Dune_file.Executables.t
        ]
        Module_name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty
    ; executables = String.Map.empty
    ; rev_map = Module_name.Map.empty
    }

  let make (libs, exes) =
    let libraries =
      match
        Lib_name.Map.of_list_map libs ~f:(fun (lib, m, obj_dir) ->
            (Library.best_name lib, (m, obj_dir)))
      with
      | Ok x -> x
      | Error (name, _, (lib2, _, _)) ->
        User_error.raise ~loc:lib2.buildable.loc
          [ Pp.textf "Library %S appears for the second time in this directory"
              (Lib_name.to_string name)
          ]
    in
    let executables =
      match
        String.Map.of_list_map exes
          ~f:(fun ((exes : Executables.t), m, obj_dir) ->
            (snd (List.hd exes.names), (m, obj_dir)))
      with
      | Ok x -> x
      | Error (name, _, (exes2, _, _)) ->
        User_error.raise ~loc:exes2.buildable.loc
          [ Pp.textf
              "Executable %S appears for the second time in this directory" name
          ]
    in
    let rev_map =
      let rev_modules =
        let by_name buildable =
          Modules.fold_user_available ~init:[] ~f:(fun m acc ->
              (Module.name m, buildable) :: acc)
        in
        List.rev_append
          (List.concat_map libs ~f:(fun (l, m, _) -> by_name (`Library l) m))
          (List.concat_map exes ~f:(fun (e, m, _) -> by_name (`Executables e) m))
      in
      match Module_name.Map.of_list rev_modules with
      | Ok x -> x
      | Error (name, _, _) ->
        let open Module_name.Infix in
        let locs =
          List.filter_map rev_modules ~f:(fun (n, b) ->
              let buildable : Dune_file.Buildable.t =
                match b with
                | `Library l -> l.buildable
                | `Executables e -> e.buildable
              in
              Option.some_if (n = name) buildable.loc)
          |> List.sort ~compare:Loc.compare
        in
        User_error.raise
          ~loc:(Loc.drop_position (List.hd locs))
          [ Pp.textf "Module %S is used in several stanzas:"
              (Module_name.to_string name)
          ; Pp.enumerate locs ~f:(fun loc ->
                Pp.verbatim (Loc.to_file_colon_line loc))
          ; Pp.text
              "To fix this error, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this dune file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable."
          ]
    in
    { libraries; executables; rev_map }
end

module Artifacts = struct
  type t =
    { libraries : Lib_info.local Lib_name.Map.t
    ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty; modules = Module_name.Map.empty }

  let lookup_module { modules; libraries = _ } = Module_name.Map.find modules

  let lookup_library { libraries; modules = _ } = Lib_name.Map.find libraries

  let make ~dir ~lib_config (libs, exes) =
    let+ libraries =
      Memo.List.map libs ~f:(fun (lib, _, _) ->
          let name = Lib_name.of_local lib.Library.name in
          let+ info = Dune_file.Library.to_lib_info lib ~dir ~lib_config in
          (name, info))
      >>| Lib_name.Map.of_list_exn
    in
    let modules =
      let by_name modules obj_dir =
        Modules_group.fold_user_available ~init:modules ~f:(fun m modules ->
            Module_name.Map.add_exn modules (Module.name m) (obj_dir, m))
      in
      let init =
        List.fold_left exes ~init:Module_name.Map.empty
          ~f:(fun modules (_, m, obj_dir) -> by_name modules obj_dir m)
      in
      List.fold_left libs ~init ~f:(fun modules (_, m, obj_dir) ->
          by_name modules obj_dir m)
    in
    { libraries; modules }
end

type t =
  { modules : Modules.t
  ; artifacts : Artifacts.t Memo.Lazy.t
  }

let empty =
  { modules = Modules.empty; artifacts = Memo.Lazy.of_val Artifacts.empty }

let artifacts t = Memo.Lazy.force t.artifacts

let modules_of_files ~dialects ~dir ~files =
  let dir = Path.build dir in
  let impl_files, intf_files =
    let make_module dialect name fn =
      (name, Module.File.make dialect (Path.relative dir fn))
    in
    let loc = Loc.in_dir dir in
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
           (* we aren't using Filename.extension because we want to handle
              filenames such as foo.cppo.ml *)
           match String.lsplit2 fn ~on:'.' with
           | None -> Skip
           | Some (s, ext) -> (
             match Dialect.DB.find_by_extension dialects ("." ^ ext) with
             | None -> Skip
             | Some (dialect, ml_kind) -> (
               let name = Module_name.of_string_allow_invalid (loc, s) in
               let module_ = make_module dialect name fn in
               match ml_kind with
               | Impl -> Left module_
               | Intf -> Right module_)))
  in
  let parse_one_set (files : (Module_name.t * Module.File.t) list) =
    match Module_name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      User_error.raise
        [ Pp.textf "Too many files for module %s in %s:"
            (Module_name.to_string name)
            (Path.Source.to_string_maybe_quoted src_dir)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f1))
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted (Module.File.path f2))
        ]
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module_name.Map.merge impls intfs ~f:(fun name impl intf ->
      Some (Module.Source.make name ?impl ?intf))

type for_ =
  | Library of Lib_name.t
  | Exe of { first_exe : string }

let modules_and_obj_dir t ~for_ =
  match for_ with
  | Library name -> Lib_name.Map.find_exn t.modules.libraries name
  | Exe { first_exe } -> String.Map.find_exn t.modules.executables first_exe

let modules t ~for_ = modules_and_obj_dir t ~for_ |> fst

let lookup_stanza_module (t : t) name =
  Module_name.Map.find t.modules.rev_map name

let lookup_module t name =
  lookup_stanza_module t name
  |> Option.map ~f:(function
       | `Library (l : Dune_file.Library.t) -> l.buildable
       | `Executables (e : Dune_file.Executables.t) -> e.buildable)

let virtual_modules lookup_vlib vlib =
  let info = Lib.info vlib in
  let+ modules =
    match Option.value_exn (Lib_info.virtual_ info) with
    | External modules -> Memo.return modules
    | Local ->
      let src_dir = Lib_info.src_dir info |> Path.as_in_build_dir_exn in
      let+ t = lookup_vlib ~dir:src_dir in
      modules t ~for_:(Library (Lib.name vlib))
  in
  let existing_virtual_modules = Modules_group.virtual_module_names modules in
  let allow_new_public_modules =
    Modules_group.wrapped modules |> Wrapped.to_bool |> not
  in
  { Modules_field_evaluator.Implementation.existing_virtual_modules
  ; allow_new_public_modules
  }

let make_lib_modules ~dir ~libs ~lookup_vlib ~(lib : Library.t) ~modules =
  let open Resolve.Memo.O in
  let+ kind, main_module_name, wrapped =
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
      let* resolved =
        let name = Library.best_name lib in
        Lib.DB.find_even_when_hidden libs name
        (* can't happen because this library is defined using the current
           stanza *)
        >>| Option.value_exn
      in
      let open Resolve.Memo.O in
      (* This [Option.value_exn] is correct because the above [lib.implements]
         is [Some _] and this [lib] variable correspond to the same library. *)
      let* vlib = Option.value_exn (Lib.implements resolved) in
      let* wrapped = Lib.wrapped resolved in
      let wrapped = Option.value_exn wrapped in
      let* main_module_name = Lib.main_module_name resolved in
      let+ impl = Resolve.Memo.lift_memo (virtual_modules lookup_vlib vlib) in
      let kind : Modules_field_evaluator.kind = Implementation impl in
      (kind, main_module_name, wrapped)
  in
  let modules =
    Modules_field_evaluator.eval ~modules ~buildable:lib.buildable ~kind
      ~private_modules:
        (Option.value ~default:Ordered_set_lang.standard lib.private_modules)
      ~src_dir:dir
  in
  let stdlib = lib.stdlib in
  let implements = Option.is_some lib.implements in
  let _loc, lib_name = lib.name in
  Modules_group.lib ~stdlib ~implements ~lib_name ~src_dir:dir ~modules
    ~main_module_name ~wrapped

let libs_and_exes dune_file ~dir ~scope ~lookup_vlib ~modules =
  Memo.parallel_map dune_file.stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        (* jeremiedimino: this [Resolve.get] means that if the user writes an
           invalid [implements] field, we will get an error immediately even if
           the library is not built. We should change this to carry the
           [Or_exn.t] a bit longer. *)
        let+ modules =
          make_lib_modules ~dir ~libs:(Scope.libs scope) ~lookup_vlib ~modules
            ~lib
          >>= Resolve.read_memo
        in
        let obj_dir = Library.obj_dir lib ~dir in
        List.Left (lib, modules, obj_dir)
      | Executables exes | Tests { exes; _ } ->
        let modules =
          let modules =
            Modules_field_evaluator.eval ~modules ~buildable:exes.buildable
              ~kind:Modules_field_evaluator.Exe_or_normal_lib
              ~private_modules:Ordered_set_lang.standard ~src_dir:dir
          in
          let project = Scope.project scope in
          if Dune_project.wrapped_executables project then
            Modules_group.exe_wrapped ~src_dir:dir ~modules
          else Modules_group.exe_unwrapped modules
        in
        let obj_dir = Dune_file.Executables.obj_dir ~dir exes in
        let modules =
          let src_dir = Path.build (Obj_dir.obj_dir obj_dir) in
          (* We need to relocate the source of the alias module to its own
             directory for executables. This module always has the same name for
             executables, therefore it might collide with ether alias modules if
             there are multiple executable stanzas in the same directory *)
          Modules_group.relocate_alias_module modules ~src_dir
        in
        Memo.return (List.Right (exes, modules, obj_dir))
      | _ -> Memo.return List.Skip)
  >>| List.filter_partition_map ~f:Fun.id

let check_no_qualified (loc, include_subdirs) =
  if include_subdirs = Dune_file.Include_subdirs.Include Qualified then
    User_error.raise ~loc
      [ Pp.text "(include_subdirs qualified) is not supported yet" ]

let make dune_file ~dir ~scope ~lib_config ~loc ~lookup_vlib ~include_subdirs
    ~dirs =
  let+ libs_and_exes =
    check_no_qualified include_subdirs;
    let modules =
      let dialects = Dune_project.dialects (Scope.project scope) in
      List.fold_left dirs ~init:Module_name.Map.empty
        ~f:(fun acc ((dir : Path.Build.t), _local, files) ->
          let modules = modules_of_files ~dialects ~dir ~files in
          Module_name.Map.union acc modules ~f:(fun name x y ->
              User_error.raise ~loc
                [ Pp.textf "Module %S appears in several directories:"
                    (Module_name.to_string name)
                ; Pp.textf "- %s"
                    (Path.to_string_maybe_quoted (Module.Source.src_dir x))
                ; Pp.textf "- %s"
                    (Path.to_string_maybe_quoted (Module.Source.src_dir y))
                ; Pp.text "This is not allowed, please rename one of them."
                ]))
    in
    libs_and_exes dune_file ~dir ~scope ~lookup_vlib ~modules
  in
  let modules = Modules.make libs_and_exes in
  let artifacts =
    Memo.lazy_ (fun () -> Artifacts.make ~dir ~lib_config libs_and_exes)
  in
  { modules; artifacts }
