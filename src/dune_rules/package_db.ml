open Import
open Memo.O

type t = Context_name.t

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t
  | Build of unit Action_builder.t * Path.t Install.Paths.t

let find_package ctx pkg =
  let* packages = Dune_load.packages () in
  match Package.Name.Map.find packages pkg with
  | Some p -> Memo.return (Some (Local p))
  | None ->
    Pkg_rules.lock_dir_active ctx
    >>= (function
     | true ->
       Pkg_rules.find_package ctx pkg
       >>| (function
        | None -> None
        | Some (b, install_paths) -> Some (Build (b, install_paths)))
     | false ->
       let* findlib = Findlib.create ctx in
       Findlib.find_root_package findlib pkg
       >>= (function
        | Ok p -> Memo.return @@ Some (Installed p)
        | Error (Invalid_dune_package user_message) ->
          User_error.raise [ User_message.pp user_message ]
        | Error Not_found -> Memo.return None))
;;

let create ctx = Memo.return ctx

let package_install ~(context : Build_context.t) ~(pkg : Package.t) =
  let dir = Path.Build.append_source context.build_dir (Package.dir pkg) in
  let name = Package.name pkg in
  sprintf ".%s-files" (Package.Name.to_string name)
  |> Alias.Name.of_string
  |> Alias.make ~dir
;;

let resolve_package_section ctx pkg_name section =
  find_package ctx pkg_name
  >>| function
  | None -> None
  | Some (Local pkg) ->
    let path = Install.Paths.get_local_location ctx section pkg_name in
    let context = Build_context.create ~name:ctx in
    let dep = Alias_builder.alias (package_install ~context ~pkg) in
    Some (path, dep)
  | Some (Build (build, install_paths)) ->
    let path = Install.Paths.get install_paths section in
    Some (path, build)
  | Some (Installed pkg) ->
    let prefix = Path.parent_exn (Path.parent_exn pkg.dir) in
    let roots = Install.Roots.opam_from_prefix prefix ~relative:Path.relative in
    let paths = Install.Paths.make ~relative:Path.relative ~package:pkg_name ~roots in
    let path = Install.Paths.get paths section in
    Some (path, Action_builder.path pkg.dir)
;;

let section_of_any_package_site any_package pkg_name loc site =
  let sites =
    match any_package with
    | Build _ ->
      (* TODO We should be able to extract this information after the package
         is built *)
      Site.Map.empty
    | Local p -> Package.sites p
    | Installed p -> p.sites
  in
  match Site.Map.find sites site with
  | Some section -> section
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf
          "Package %s doesn't define a site %s"
          (Package.Name.to_string pkg_name)
          (Site.to_string site)
      ]
;;

let section_of_site t ~loc ~pkg:pkg_name ~site =
  find_package t pkg_name
  >>| function
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf "The package %s is not found" (Package.Name.to_string pkg_name) ]
  | Some pkg -> section_of_any_package_site pkg pkg_name loc site
;;
