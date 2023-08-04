open Import

type t =
  | Section of Section.t
  | Site of
      { pkg : Package.Name.t
      ; site : Site.t
      ; loc : Loc.t
      }

let to_dyn x =
  let open Dyn in
  match x with
  | Section s -> variant "Section" [ Section.to_dyn s ]
  | Site { pkg; site; loc = _ } ->
    variant "Section" [ Package.Name.to_dyn pkg; Site.to_dyn site ]
;;

let to_string = function
  | Section s -> Section.to_string s
  | Site { pkg; site; loc = _ } ->
    sprintf "(site %s %s)" (Package.Name.to_string pkg) (Site.to_string site)
;;

let decode =
  let open Dune_lang.Decoder in
  sum
    ((Section.enum_decoder |> List.map ~f:(fun (k, d) -> k, return (Section d)))
     @ [ ( "site"
         , Dune_lang.Syntax.since Site.dune_site_syntax (0, 1)
           >>> located (pair Package.Name.decode Site.decode)
           >>| fun (loc, (pkg, site)) -> Site { pkg; site; loc } )
       ])
;;

let encode =
  let open Dune_lang.Encoder in
  function
  | Section s -> Section.encode s
  | Site { pkg; site; loc = _ } ->
    constr "site" (pair Package.Name.encode Site.encode) (pkg, site)
;;
