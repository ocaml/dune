open Stdune
open Dune_file
open Build_system

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

  let of_cm_kind = function
    | Cm_kind.Cmx -> Cmx
    | Cmi -> Cmi
    | Cmo -> Exn.code_error "Lib_file_deps.Group.of_cm_kind: Cmo" []

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

  module L = struct
    let to_string l =
      List.map l ~f:to_string
      |> List.sort ~compare:String.compare
      |> String.concat  ~sep:"-and-"

    let alias t ~dir ~name =
      sprintf "lib-%s%s-all" (Lib_name.to_string name) (to_string t)
      |> Alias.make ~dir

    let setup_alias t ~dir ~lib ~files =
      Build_system.Alias.add_deps
        (alias t ~dir ~name:(Library.best_name lib))
        files

    let setup_file_deps_group_alias t ~dir ~lib =
      setup_alias t ~dir ~lib ~files:(
        List.map t ~f:(fun t ->
          Alias.stamp_file (alias [t] ~dir ~name:(Library.best_name lib)))
        |> Path.Set.of_list
      )
  end
end

let setup_file_deps =
  let cm_kinds = [Cm_kind.Cmx; Cmi] in
  let groups = List.map ~f:Group.of_cm_kind cm_kinds in
  fun ~dir ~lib ~modules ->
    let add_cms ~cm_kind =
      List.fold_left ~f:(fun acc m ->
        match Module.cm_public_file m cm_kind with
        | None -> acc
        | Some fn -> Path.Set.add acc fn)
    in
    List.iter cm_kinds ~f:(fun cm_kind ->
      let files = add_cms ~cm_kind ~init:Path.Set.empty modules in
      let groups = [Group.of_cm_kind cm_kind] in
      Group.L.setup_alias groups ~dir ~lib ~files);
    Group.L.setup_file_deps_group_alias groups ~dir ~lib;
    Group.L.setup_alias [Header] ~dir ~lib ~files:(
      List.map lib.install_c_headers ~f:(fun header ->
        Path.relative dir (header ^ ".h"))
      |> Path.Set.of_list)

let deps_of_lib (lib : Lib.t) ~groups =
  if Lib.is_local lib then
    Group.L.alias groups ~dir:(Lib.src_dir lib) ~name:(Lib.name lib)
    |> Alias.stamp_file
    |> Dep.file
    |> Dep.Set.singleton
  else
    (* suppose that all the files of an external lib are at the same place *)
    let dir = Obj_dir.public_cmi_dir (Lib.obj_dir lib) in
    List.map groups ~f:(fun g -> Dep.glob ~dir (Group.to_predicate g))
    |> Dep.Set.of_list

let deps_with_exts =
  List.fold_left ~init:Dep.Set.empty ~f:(fun acc (lib, groups) ->
    let deps = deps_of_lib lib ~groups in
    Dep.Set.union acc deps)

let deps libs ~groups =
  List.fold_left ~init:Dep.Set.empty libs ~f:(fun acc lib ->
    let deps = deps_of_lib lib ~groups in
    Dep.Set.union acc deps)
