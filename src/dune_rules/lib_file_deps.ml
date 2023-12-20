open Import

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
        let pred =
          Glob.matching_extensions [ String.drop_prefix_if_exists ~prefix:"." (ext g) ]
        in
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

let deps_with_exts = Dep.Set.union_map ~f:(fun (lib, groups) -> deps_of_lib lib ~groups)
let deps libs ~groups = Dep.Set.union_map libs ~f:(deps_of_lib ~groups)

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
  let open Memo.O in
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
