open Import
open Memo.O

type t = Context_name.t

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t
  | Build of unit Action_builder.t

let find_package ctx pkg =
  let* packages = Only_packages.get () in
  match Package.Name.Map.find packages pkg with
  | Some p -> Memo.return (Some (Local p))
  | None ->
    let open Memo.O in
    let* findlib = Findlib.create ctx in
    Findlib.find_root_package findlib pkg
    >>= (function
    | Ok p -> Memo.return @@ Some (Installed p)
    | Error (Invalid_dune_package user_message) ->
      User_error.raise [ User_message.pp user_message ]
    | Error Not_found ->
      Pkg_rules.find_package ctx pkg
      >>| (function
      | None -> None
      | Some b -> Some (Build b)))
;;

let create ctx = Memo.return ctx

let section_of_site t ~loc ~pkg ~site =
  let+ sites =
    let+ pkg = find_package t pkg in
    Option.map pkg ~f:(function
      | Build _ -> Site.Map.empty
      | Local p -> p.sites
      | Installed p -> p.sites)
  in
  match sites with
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf "The package %s is not found" (Package.Name.to_string pkg) ]
  | Some sites ->
    (match Site.Map.find sites site with
     | Some section -> section
     | None ->
       User_error.raise
         ~loc
         [ Pp.textf
             "Package %s doesn't define a site %s"
             (Package.Name.to_string pkg)
             (Site.to_string site)
         ])
;;
