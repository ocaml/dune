open Import
open Memo.O

module Scope_key = struct
  let file_key project =
    let name = Dune_project.name project in
    let root = Dune_project.root project in
    let digest =
      Digest.repr Repr.(pair Dune_project_name.repr Path.Source.repr) (name, root)
      |> Digest.to_string
    in
    String.take digest 12
  ;;

  let find_project_by_key =
    let memo =
      let make_map projects =
        String.Map.of_list_map_exn projects ~f:(fun project -> file_key project, project)
        |> Memo.return
      in
      let module Input = struct
        type t = Dune_project.t list

        let equal = List.equal Dune_project.equal
        let hash = List.hash Dune_project.hash
        let to_dyn = Dyn.list Dune_project.to_dyn
      end
      in
      Memo.create "project-by-keys" ~input:(module Input) make_map
    in
    fun key ->
      let* projects = Dune_load.projects () in
      let+ map = Memo.exec memo projects in
      String.Map.find_exn map key
  ;;

  let parse_priv_lib_name s = String.rsplit2 s ~on:'@'

  let find_private_lib context s =
    match parse_priv_lib_name s with
    | None -> Memo.return None
    | Some (lib, key) ->
      let* scope = find_project_by_key key >>= Scope.DB.find_by_project context in
      Lib.DB.find (Scope.libs scope) (Lib_name.parse_string_exn (Loc.none, lib))
  ;;

  let of_string context s =
    match parse_priv_lib_name s with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      Lib_name.parse_string_exn (Loc.none, s), public_libs
    | Some (lib, key) ->
      let+ scope = find_project_by_key key >>= Scope.DB.find_by_project context in
      Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope
  ;;

  let to_string lib project =
    let key = file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib) key
  ;;
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private _ | Installed _ ->
    Code_error.raise
      "Odoc_scope.lib_unique_name: installed library"
      [ "name", Lib_name.to_dyn name ]
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;

module Parent_id = struct
  type t = string

  let to_string = Fun.id
end

let pkg_parent_id pkg = Package.Name.to_string pkg

let lib_parent_id lib =
  let name = Lib.name lib in
  let in_package pkg name =
    sprintf "%s/%s" (pkg_parent_id pkg) (Lib_name.to_string name)
  in
  match Lib_info.status (Lib.info lib) with
  | Installed_private pkg | Installed pkg ->
    in_package (Option.value pkg ~default:(Lib_name.package_name name)) name
  | Public (_, pkg) -> in_package (Package.name pkg) name
  | Private (_, Some pkg) ->
    let pkg = Package.name pkg in
    in_package pkg (Lib_name.mangled pkg (Lib_name.to_local_exn name))
  | Private (project, None) -> Scope_key.to_string name project
;;
