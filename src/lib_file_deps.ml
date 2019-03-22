open Stdune

module Group = struct
  type t =
    | Cmi
    | Cmx
    | Header

  let all = [Cmi; Cmx; Header]

  let to_string = function
    | Cmi -> ".cmi"
    | Cmx -> ".cmx"
    | Header -> ".h"

  let obj_dir t obj_dir =
    match t with
    | Cmi -> Obj_dir.public_cmi_dir obj_dir
    | Cmx -> Obj_dir.native_dir obj_dir
    | Header -> Obj_dir.dir obj_dir

  let to_predicate =
    let preds = List.map all ~f:(fun g ->
      let ext = to_string g in
      (* we cannot use globs because of bootstrapping. *)
      let id = lazy (
        let open Sexp.Encoder in
        constr "Lib_file_deps" [Atom ext]
      ) in
      let pred = Predicate.create ~id ~f:(fun p ->
        String.equal (Path.extension p) ext)
      in
      (g, pred))
    in
    fun g -> Option.value_exn (List.assoc preds g)
end

let deps_of_lib (lib : Lib.t) ~groups =
  let obj_dir = Lib.obj_dir lib in
  List.map groups ~f:(fun g ->
    let dir = Group.obj_dir g obj_dir in
    Dep.glob ~dir (Group.to_predicate g))
  |> Dep.Set.of_list

let deps_with_exts =
  Dep.Set.union_map ~f:(fun (lib, groups) -> deps_of_lib lib ~groups)

let deps libs ~groups = Dep.Set.union_map libs ~f:(deps_of_lib ~groups)
