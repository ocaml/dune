open Import
open Memo.O

module Group = struct
  type ocaml =
    | Cmi
    | Cmx

  type t =
    | Ocaml of ocaml
    | Melange of Melange.Cm_kind.t
    | Header

  let all = [ Ocaml Cmi; Ocaml Cmx; Melange Cmi; Melange Cmj; Header ]

  let ext = function
    | Ocaml Cmi -> Cm_kind.ext Cmi
    | Ocaml Cmx -> Cm_kind.ext Cmx
    | Melange Cmi -> Lib_mode.Cm_kind.ext (Melange Cmi)
    | Melange Cmj -> Lib_mode.Cm_kind.ext (Melange Cmj)
    | Header -> Foreign_language.header_extension
  ;;

  let obj_dir t obj_dir =
    match t with
    | Ocaml Cmi -> Obj_dir.public_cmi_ocaml_dir obj_dir
    | Ocaml Cmx -> Obj_dir.native_dir obj_dir
    | Melange Cmi -> Obj_dir.public_cmi_melange_dir obj_dir
    | Melange Cmj -> Obj_dir.melange_dir obj_dir
    | Header -> Obj_dir.dir obj_dir
  ;;

  let to_glob =
    let preds =
      List.map all ~f:(fun g ->
        let pred = Glob.matching_extensions [ ext g ] in
        g, pred)
    in
    fun g -> Option.value_exn (List.assoc preds g)
  ;;
end

let deps_of_lib (lib : Lib.t) ~groups =
  let obj_dir = Lib.info lib |> Lib_info.obj_dir in
  List.map groups ~f:(fun g ->
    let dir = Group.obj_dir g obj_dir in
    Group.to_glob g |> File_selector.of_glob ~dir |> Dep.file_selector)
  |> Dep.Set.of_list
;;

let deps_of_module (lib : Lib.t) (m : Module.t) ~cm_kinds =
  let obj_dir = Lib.info lib |> Lib_info.obj_dir in
  List.filter_map cm_kinds ~f:(fun kind ->
    Obj_dir.Module.cm_public_file obj_dir m ~kind |> Option.map ~f:(fun p -> Dep.file p))
  |> Dep.Set.of_list
;;

let deps libs ~groups = Dep.Set.union_map libs ~f:(deps_of_lib ~groups)

let deps_of_entries ~opaque ~(cm_kind : Lib_mode.Cm_kind.t) entries =
  let groups, cm_kinds =
    match cm_kind with
    | Ocaml Cmi | Ocaml Cmo -> [ Group.Ocaml Cmi ], [ Lib_mode.Cm_kind.Ocaml Cmi ]
    | Melange Cmi -> [ Group.Melange Cmi ], [ Melange Cmi ]
    | Melange Cmj -> [ Group.Melange Cmi; Melange Cmj ], [ Melange Cmi; Melange Cmj ]
    | Ocaml Cmx -> [ Group.Ocaml Cmi; Ocaml Cmx ], [ Ocaml Cmi; Ocaml Cmx ]
  in
  List.map entries ~f:(fun ((lib : Lib.t), mod_opt) ->
    let is_opaque_local =
      match cm_kind with
      | Ocaml Cmx -> opaque && Lib.is_local lib
      | _ -> false
    in
    match mod_opt with
    | Some m ->
      let cm_kinds =
        if is_opaque_local then [ Lib_mode.Cm_kind.Ocaml Cmi ] else cm_kinds
      in
      deps_of_module lib m ~cm_kinds
    | None ->
      let groups = if is_opaque_local then [ Group.Ocaml Cmi ] else groups in
      deps_of_lib lib ~groups)
  |> List.fold_left ~init:Dep.Set.empty ~f:Dep.Set.union
;;

type path_specification =
  | Allow_all
  | Disallow_external of Lib_name.t

let raise_disallowed_external_path ~loc lib_name path =
  User_error.raise
    ~loc
    [ Pp.textf
        "Public library %s depends on external path `%s'. This is not allowed."
        (Lib_name.to_string lib_name)
        (Path.to_string path)
    ]
    ~hints:
      [ Pp.textf "Move the external dependency to the workspace and use a relative path."
      ]
;;

let eval ~loc ~expander ~paths:path_spec (deps : Dep_conf.t list) =
  let runtime_deps, sandbox = Dep_conf_eval.unnamed_get_paths ~expander deps in
  Option.iter sandbox ~f:(fun _ ->
    User_error.raise ~loc [ Pp.text "sandbox settings are not allowed" ]);
  let+ paths, _ = Action_builder.evaluate_and_collect_deps runtime_deps in
  (match path_spec with
   | Allow_all -> ()
   | Disallow_external lib_name ->
     Path.Set.iter paths ~f:(fun path ->
       match Path.as_external path with
       | None -> ()
       | Some _ -> raise_disallowed_external_path ~loc lib_name path));
  paths
;;

module Lib_index = struct
  (** Each entry pairs a library with an optional Module.t: None means use
      glob deps (wrapped libs, external unwrapped), Some m means use
      per-file deps via Obj_dir.Module.cm_file (local unwrapped). *)
  type entry = Lib.t * Module.t option

  type t =
    { by_module_name : entry list Module_name.Map.t
    ; unresolved : Lib.t list
    }

  let module_names_of_lib sctx (lib : Lib.t) ~for_
    : (Module_name.t * Module.t option) list option Resolve.Memo.t
    =
    let open Resolve.Memo.O in
    let* main_module = Lib.main_module_name lib in
    match main_module with
    | Some name ->
      (* Wrapped library: index the wrapper name plus all inner module
         names so that references via -open flags are resolved. All map
         to None (glob deps) since the wrapper exposes the whole lib. *)
      let info = Lib.info lib in
      (match Lib_info.entry_modules info ~for_ with
       | Lib_info.Source.External _ ->
         (match Lib_info.modules info ~for_ with
          | Lib_info.Source.External (Some modules_with_vlib) ->
            let modules = Modules.With_vlib.drop_vlib modules_with_vlib in
            let inner_names =
              Modules.fold modules ~init:[] ~f:(fun m acc -> (Module.name m, None) :: acc)
            in
            Resolve.Memo.return (Some ((name, None) :: inner_names))
          | _ -> Resolve.Memo.return (Some [ name, None ]))
       | Lib_info.Source.Local ->
         Resolve.Memo.lift_memo
           (let open Memo.O in
            let+ modules_opt = Dir_contents.modules_of_lib sctx lib ~for_ in
            match modules_opt with
            | None -> Some [ name, None ]
            | Some modules_with_vlib ->
              let modules = Modules.With_vlib.drop_vlib modules_with_vlib in
              let inner_names =
                Modules.fold modules ~init:[] ~f:(fun m acc ->
                  (Module.name m, None) :: acc)
              in
              Some ((name, None) :: inner_names)))
    | None ->
      let info = Lib.info lib in
      (match Lib_info.entry_modules info ~for_ with
       | Lib_info.Source.External (Ok names) ->
         Resolve.Memo.return (Some (List.map names ~f:(fun n -> n, None)))
       | Lib_info.Source.External (Error _) -> Resolve.Memo.return None
       | Lib_info.Source.Local ->
         Resolve.Memo.lift_memo
           (let open Memo.O in
            let+ modules_opt = Dir_contents.modules_of_lib sctx lib ~for_ in
            match modules_opt with
            | None -> None
            | Some modules_with_vlib ->
              let modules = Modules.With_vlib.drop_vlib modules_with_vlib in
              let entry_modules = Modules.entry_modules modules in
              Some (List.map entry_modules ~f:(fun m -> Module.name m, Some m))))
  ;;

  let empty = { by_module_name = Module_name.Map.empty; unresolved = [] }

  let create sctx (libs : Lib.t list) ~for_ : t Resolve.Memo.t =
    let open Resolve.Memo.O in
    let+ entries =
      Resolve.Memo.List.map libs ~f:(fun lib ->
        let+ names_opt = module_names_of_lib sctx lib ~for_ in
        lib, names_opt)
    in
    let by_module_name, unresolved =
      List.fold_left
        entries
        ~init:(Module_name.Map.empty, [])
        ~f:(fun (by_name, unresolved) (lib, names_opt) ->
          match names_opt with
          | None -> by_name, lib :: unresolved
          | Some named_modules ->
            let by_name =
              List.fold_left named_modules ~init:by_name ~f:(fun acc (name, mod_opt) ->
                Module_name.Map.update acc name ~f:(function
                  | None -> Some [ lib, mod_opt ]
                  | Some entries -> Some ((lib, mod_opt) :: entries)))
            in
            by_name, unresolved)
    in
    { by_module_name; unresolved }
  ;;

  let filter_libs (index : t) ~(referenced_modules : Module_name.Set.t) : entry list =
    let from_refs =
      Module_name.Set.fold referenced_modules ~init:[] ~f:(fun name acc ->
        match Module_name.Map.find index.by_module_name name with
        | None -> acc
        | Some entries -> List.rev_append entries acc)
    in
    let unresolved = List.map index.unresolved ~f:(fun lib -> lib, None) in
    (* Entries are unique per (lib, module name). Two modules with the
       same name can't appear for the same library since module names are
       unique within a library's entry modules. *)
    let compare (a_lib, a_mod) (b_lib, b_mod) =
      match Lib.compare a_lib b_lib with
      | (Lt | Gt) as c -> c
      | Eq ->
        (match a_mod, b_mod with
         | None, None -> Eq
         | None, Some _ -> Lt
         | Some _, None -> Gt
         | Some a, Some b -> Module_name.compare (Module.name a) (Module.name b))
    in
    List.rev_append unresolved from_refs |> List.sort_uniq ~compare
  ;;
end
