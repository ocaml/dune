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

  let obj_dir t obj_dir =
    match t with
    | Ocaml Cmi -> Obj_dir.public_cmi_dir obj_dir
    | Ocaml Cmx -> Obj_dir.native_dir obj_dir
    | Melange (Cmi | Cmj) -> Obj_dir.melange_dir obj_dir
    | Header -> Obj_dir.dir obj_dir

  let to_predicate =
    let preds =
      List.map all ~f:(fun g ->
          let ext = ext g in
          (* we cannot use globs because of bootstrapping. *)
          let id =
            lazy
              (let open Dyn in
              variant "Lib_file_deps" [ string ext ])
          in
          let pred =
            Predicate_with_id.create ~id ~f:(fun p ->
                String.equal (Filename.extension p) ext)
          in
          (g, pred))
    in
    fun g -> Option.value_exn (List.assoc preds g)
end

let deps_of_lib (lib : Lib.t) ~groups =
  let obj_dir = Lib.info lib |> Lib_info.obj_dir in
  List.map groups ~f:(fun g ->
      let dir = Group.obj_dir g obj_dir in
      Group.to_predicate g |> File_selector.create ~dir |> Dep.file_selector)
  |> Dep.Set.of_list

let deps_with_exts =
  Dep.Set.union_map ~f:(fun (lib, groups) -> deps_of_lib lib ~groups)

let deps libs ~groups = Dep.Set.union_map libs ~f:(deps_of_lib ~groups)
