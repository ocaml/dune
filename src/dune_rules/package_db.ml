open Import
open Memo.O

type t = Context_name.t

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t
  | Build of unit Action_builder.t

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
        | Some b -> Some (Build b))
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
