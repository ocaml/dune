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

(* Currently unused: per-file deps on individual modules within a library.
   Retained for potential future use when per-module filtering of unwrapped
   libraries is supported. *)
let deps_of_module (lib : Lib.t) (m : Module.t) ~cm_kinds =
  let obj_dir = Lib.info lib |> Lib_info.obj_dir in
  List.filter_map cm_kinds ~f:(fun kind ->
    Obj_dir.Module.cm_public_file obj_dir m ~kind |> Option.map ~f:(fun p -> Dep.file p))
  |> Dep.Set.of_list
;;

let deps_of_entries ~opaque ~(cm_kind : Lib_mode.Cm_kind.t) entries =
  let groups_for lib =
    match cm_kind with
    | Ocaml Cmi | Ocaml Cmo -> [ Group.Ocaml Cmi ]
    | Ocaml Cmx ->
      if opaque && Lib.is_local lib
      then [ Group.Ocaml Cmi ]
      else [ Group.Ocaml Cmi; Group.Ocaml Cmx ]
    | Melange Cmi -> [ Group.Melange Cmi ]
    | Melange Cmj -> [ Group.Melange Cmi; Group.Melange Cmj ]
  in
  (* Currently unused: [cm_kinds_for] and the [Some m] branch below support
     per-file deps on individual modules. All entries currently use [None]
     (glob deps), so this path is dead code. Retained for future use. *)
  let cm_kinds_for lib =
    match cm_kind with
    | Ocaml Cmi | Ocaml Cmo -> [ Lib_mode.Cm_kind.Ocaml Cmi ]
    | Ocaml Cmx ->
      if opaque && Lib.is_local lib
      then [ Lib_mode.Cm_kind.Ocaml Cmi ]
      else [ Lib_mode.Cm_kind.Ocaml Cmi; Ocaml Cmx ]
    | Melange Cmi -> [ Lib_mode.Cm_kind.Melange Cmi ]
    | Melange Cmj -> [ Lib_mode.Cm_kind.Melange Cmi; Melange Cmj ]
  in
  Dep.Set.union_map entries ~f:(fun (lib, module_opt) ->
    match module_opt with
    | None -> deps_of_lib lib ~groups:(groups_for lib)
    | Some m -> deps_of_module lib m ~cm_kinds:(cm_kinds_for lib))
;;

module Lib_index = struct
  type entry = Lib.t * Module.t option
  type t = { by_module_name : entry list Module_name.Map.t }

  let empty = { by_module_name = Module_name.Map.empty }

  let create entries =
    let by_module_name =
      List.fold_left entries ~init:Module_name.Map.empty ~f:(fun map (name, entry) ->
        Module_name.Map.update map name ~f:(function
          | None -> Some [ entry ]
          | Some entries -> Some (entry :: entries)))
    in
    { by_module_name }
  ;;

  let filter_libs t ~referenced_modules =
    Module_name.Set.fold referenced_modules ~init:[] ~f:(fun name acc ->
      match Module_name.Map.find t.by_module_name name with
      | None -> acc
      | Some entries -> List.rev_append entries acc)
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
