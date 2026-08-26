open Import
open Memo.O

type t =
  { libraries : Lib_info.local Lib_name.Map.t
  ; modules_by_source_path : (Path.Build.t Obj_dir.t * Module.t) Path.Build.Map.t
  ; modules_by_logical_path :
      (Path.Build.t Obj_dir.t * Module.t) list Module_name.Path.Map.t
  ; include_subdirs : Include_subdirs.t
  ; melange_emits : Melange.Emit.t Path.Build.Map.t
  }

let empty =
  { libraries = Lib_name.Map.empty
  ; modules_by_source_path = Path.Build.Map.empty
  ; modules_by_logical_path = Module_name.Path.Map.empty
  ; include_subdirs = No
  ; melange_emits = Path.Build.Map.empty
  }
;;

let lookup_module_by_source_path { modules_by_source_path; _ } =
  Path.Build.Map.find modules_by_source_path
;;

let lookup_modules_by_logical_path { modules_by_logical_path; _ } path =
  Module_name.Path.Map.find modules_by_logical_path path |> Option.value ~default:[]
;;

let include_subdirs t = t.include_subdirs
let lookup_library { libraries; _ } = Lib_name.Map.find libraries
let lookup_melange_emit { melange_emits; _ } = Path.Build.Map.find melange_emits

(* The source file build path of a module, with the [.ml]/[.mli] extension
   stripped. Before Dune 3.25, this matches the form a user writes in a module
   artifact pform (e.g. [%{cmi:sub_a/group}] for [sub_a/group.ml]). *)
let module_source_path_without_extension m =
  let source =
    match Module.source_without_pp m ~ml_kind:Impl with
    | Some _ as p -> p
    | None -> Module.source_without_pp m ~ml_kind:Intf
  in
  Option.bind source ~f:Path.as_in_build_dir
  |> Option.map ~f:(fun p -> fst (Path.Build.split_extension p))
;;

let make ~dir ~expander ~lib_config ~libs ~exes ~include_subdirs ~melange_emits =
  let+ libraries =
    Memo.List.map libs ~f:(fun ((lib : Library.t), _, _) ->
      let+ lib_config = lib_config in
      let name = Lib_name.of_local lib.name in
      let info =
        Library.to_lib_info lib ~expander:(Memo.return expander) ~dir ~lib_config
      in
      name, info)
    >>| Lib_name.Map.of_list_exn
  in
  let modules_by_source_path, modules_by_logical_path =
    let add_modules modules obj_dir =
      Modules.fold_user_available
        ~init:modules
        ~f:(fun m (by_source_path, by_logical_path) ->
          match module_source_path_without_extension m with
          | None -> by_source_path, by_logical_path
          | Some source_path ->
            ( Path.Build.Map.add_exn by_source_path source_path (obj_dir, m)
            , Module_name.Path.Map.add_multi by_logical_path (Module.path m) (obj_dir, m)
            ))
    in
    let init =
      List.fold_left
        exes
        ~init:(Path.Build.Map.empty, Module_name.Path.Map.empty)
        ~f:(fun modules (m, obj_dir) -> add_modules modules obj_dir m)
    in
    List.fold_left libs ~init ~f:(fun modules (_, m, obj_dir) ->
      add_modules modules obj_dir m)
  in
  let melange_emits =
    match Path.Build.Map.of_list melange_emits with
    | Ok map -> Path.Build.Map.map map ~f:fst
    | Error (target_dir, (_, loc1), (_, loc2)) ->
      User_error.raise
        ~loc:loc1
        [ Pp.textf
            "Melange emit target directory %S appears more than once."
            (Path.Build.to_string target_dir)
        ; Pp.textf "Already defined at %s" (Loc.to_file_colon_line loc2)
        ]
  in
  { libraries
  ; modules_by_source_path
  ; modules_by_logical_path
  ; include_subdirs
  ; melange_emits
  }
;;
