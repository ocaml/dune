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

let deps libs ~groups = Dep.Set.union_map libs ~f:(deps_of_lib ~groups)

let groups_for_cm_kind ~opaque ~(cm_kind : Lib_mode.Cm_kind.t) lib =
  match cm_kind with
  | Ocaml Cmi | Ocaml Cmo -> [ Group.Ocaml Cmi ]
  | Ocaml Cmx ->
    if opaque && Lib.is_local lib
    then [ Group.Ocaml Cmi ]
    else [ Group.Ocaml Cmi; Group.Ocaml Cmx ]
  | Melange Cmi -> [ Group.Melange Cmi ]
  | Melange Cmj -> [ Group.Melange Cmi; Group.Melange Cmj ]
;;

let deps_of_entries ~opaque ~cm_kind libs =
  Dep.Set.union_map libs ~f:(fun lib ->
    deps_of_lib lib ~groups:(groups_for_cm_kind ~opaque ~cm_kind lib))
;;

(* [cm_public_file] gives the cmi path consumers read via their [-I]
   include path, which for libraries with a dedicated public cmi dir
   ([private_modules]) differs from the internal compilation output.
   Using it ensures the dep triggers the produce-public-cmi rule.

   Only called for local unwrapped libraries: [can_filter] rejects
   melange cm_kinds, so this function handles ocaml only. *)
let deps_of_entry_modules ~opaque ~(cm_kind : Lib_mode.Cm_kind.t) lib modules =
  let obj_dir = Lib.info lib |> Lib_info.obj_dir in
  let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
  let want_cmx =
    match cm_kind with
    | Ocaml Cmx -> not (opaque && Lib.is_local lib)
    | _ -> false
  in
  List.fold_left modules ~init:Dep.Set.empty ~f:(fun acc m ->
    let acc =
      match Obj_dir.Module.cm_public_file obj_dir m ~kind:cmi_kind with
      | Some path -> Dep.Set.add acc (Dep.file path)
      | None -> acc
    in
    if want_cmx && Module.has m ~ml_kind:Impl
    then (
      match Obj_dir.Module.cm_public_file obj_dir m ~kind:(Ocaml Cmx) with
      | Some path -> Dep.Set.add acc (Dep.file path)
      | None -> acc)
    else acc)
;;

module Lib_index = struct
  type t =
    { by_module_name : (Lib.t * Module.t option) list Module_name.Map.t
    ; tight_eligible : Lib.Set.t
    ; no_ocamldep : Lib.Set.t
      (* Local libs whose ocamldep is short-circuited by
         [Dep_rules.skip_ocamldep] — single-module stanzas without
         library dependencies have no [.d] build rules. BFS must
         not try to read ocamldep for them; their entry module
         can have no cross-library references anyway. *)
    }

  let empty =
    { by_module_name = Module_name.Map.empty
    ; tight_eligible = Lib.Set.empty
    ; no_ocamldep = Lib.Set.empty
    }
  ;;

  (* A library is eligible for per-module tight deps iff it is local
     (so every entry has a known [Module.t] with which we can call
     [Obj_dir.Module.cm_public_file]) and unwrapped (so the public
     cmi dir does not need a glob to reach internal alias modules). *)
  let is_lib_tight_eligible lib =
    Lib.is_local lib
    &&
    match Lib_info.wrapped (Lib.info lib) with
    | Some (This w) -> not (Wrapped.to_bool w)
    | Some (From _) | None ->
      (* [Some (From _)]: wrapped setting inherited from a virtual
         library. The [has_virtual_impl] branch higher up in
         [lib_deps_for_module] should handle virtual-impl contexts
         before we reach here; stay defensive.
         [None]: no wrapped information available (e.g. legacy
         [dune-package]). *)
      false
  ;;

  let create ?(no_ocamldep = Lib.Set.empty) entries =
    let by_module_name =
      List.fold_left entries ~init:Module_name.Map.empty ~f:(fun map (name, lib, m) ->
        Module_name.Map.update map name ~f:(function
          | None -> Some [ lib, m ]
          | Some xs -> Some ((lib, m) :: xs)))
    in
    let tight_eligible =
      List.fold_left entries ~init:Lib.Set.empty ~f:(fun acc (_, lib, _) ->
        if is_lib_tight_eligible lib then Lib.Set.add acc lib else acc)
    in
    { by_module_name; tight_eligible; no_ocamldep }
  ;;

  type classified =
    { tight : Module.t list Lib.Map.t
    ; non_tight : Lib.t list
    }

  let filter_libs_with_modules idx ~referenced_modules =
    let add_entry (tight, non_tight) (lib, m_opt) =
      match m_opt with
      | Some m when Lib.Set.mem idx.tight_eligible lib ->
        let tight =
          Lib.Map.update tight lib ~f:(function
            | None -> Some [ m ]
            | Some ms -> Some (m :: ms))
        in
        tight, non_tight
      | _ -> tight, Lib.Set.add non_tight lib
    in
    let tight, non_tight =
      Module_name.Set.fold
        referenced_modules
        ~init:(Lib.Map.empty, Lib.Set.empty)
        ~f:(fun name acc ->
          match Module_name.Map.find idx.by_module_name name with
          | None -> acc
          | Some entries -> List.fold_left entries ~init:acc ~f:add_entry)
    in
    { tight; non_tight = Lib.Set.to_list non_tight }
  ;;

  let lookup_tight_entries idx name =
    match Module_name.Map.find idx.by_module_name name with
    | None -> []
    | Some entries ->
      List.filter_map entries ~f:(fun (lib, m_opt) ->
        match m_opt with
        | Some m
          when Lib.Set.mem idx.tight_eligible lib && not (Lib.Set.mem idx.no_ocamldep lib)
          -> Some (lib, m)
        | _ -> None)
  ;;
end

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
