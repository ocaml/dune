open Import
open Memo.O

type t =
  { packages : Package.t Package.Name.Map.t
  ; findlib : Findlib.t
  }

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t

let find_package t pkg =
  match Package.Name.Map.find t.packages pkg with
  | Some p -> Memo.return (Some (Local p))
  | None -> (
    let open Memo.O in
    Findlib.find_root_package t.findlib pkg >>| function
    | Ok p -> Some (Installed p)
    | Error Not_found -> None
    | Error (Invalid_dune_package user_message) ->
      User_error.raise [ User_message.pp user_message ])

let create (context : Context.t) =
  let* packages = Only_packages.get () in
  let+ findlib =
    Findlib.create ~paths:context.findlib_paths ~lib_config:context.lib_config
  in
  { packages; findlib }

let section_of_site t ~loc ~pkg ~site =
  let+ sites =
    let+ pkg = find_package t pkg in
    Option.map pkg ~f:(function
      | Local p -> p.sites
      | Installed p -> p.sites)
  in
  match sites with
  | None ->
    User_error.raise ~loc
      [ Pp.textf "The package %s is not found" (Package.Name.to_string pkg) ]
  | Some sites -> (
    match Site.Map.find sites site with
    | Some section -> section
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Package %s doesn't define a site %s"
            (Package.Name.to_string pkg)
            (Site.to_string site)
        ])
