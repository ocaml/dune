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
