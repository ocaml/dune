open Stdune
module Name = Package_name

module Opam_package = struct
  type t =
    { id : Package_id.t
    ; opam_file : Path.Source.t
    ; loc : Loc.t
    ; synopsis : string option
    ; description : string option
    ; depends : OpamTypes.filtered_formula
    ; conflicts : Package_dependency.t list
    ; depopts : Package_dependency.t list
    ; info : Package_info.t
    ; version : Package_version.t option
    ; tags : string list
    }

  let create
        ~name
        ~loc
        ~version
        ~conflicts
        ~depends
        ~depopts
        ~info
        ~dir
        ~synopsis
        ~description
        ~tags
    =
    (* TODO fix *)
    let id = Package_id.create ~name ~dir in
    let opam_file = dir in
    { id
    ; opam_file
    ; loc
    ; synopsis
    ; description
    ; depends
    ; conflicts
    ; depopts
    ; info
    ; version
    ; tags
    }
  ;;

  let to_dyn _t = Dyn.string "TODO"

  let depends t = t.depends
  let depopts t = t.depopts
  let conflicts t = t.conflicts
end

type t =
  | Dune_package of Dune_package.t
  | Opam_package of Opam_package.t

let of_dune_package pkg = Dune_package pkg
let of_opam_package pkg = Opam_package pkg

let name = function
  | Dune_package t -> Dune_package.name t
  | Opam_package t -> Package_id.name t.id
;;

let info = function
  | Dune_package t -> Dune_package.info t
  | Opam_package t -> t.info
;;

let version = function
  | Dune_package t -> Dune_package.version t
  | Opam_package t -> t.version
;;

let opam_file = function
  | Dune_package t -> Dune_package.opam_file t
  | Opam_package t -> t.opam_file
;;

let tags = function
  | Dune_package t -> Dune_package.tags t
  | Opam_package t -> t.tags
;;

let sites = function
  | Dune_package t -> Dune_package.sites t
  | Opam_package _ -> Site.Map.empty
;;

let deprecated_package_names = function
  | Dune_package t -> Dune_package.deprecated_package_names t
  | Opam_package _ -> Name.Map.empty
;;

let loc = function
  | Dune_package t -> Dune_package.loc t
  | Opam_package t -> t.loc
;;

let dir = function
  | Dune_package t -> Dune_package.dir t
  | Opam_package t -> Package_id.dir t.id
;;

let synopsis = function
  | Dune_package t -> Dune_package.synopsis t
  | Opam_package t -> t.synopsis
;;

let description = function
  | Dune_package t -> Dune_package.description t
  | Opam_package t -> t.description
;;

let dune_package = function
  | Dune_package t -> Some t
  | Opam_package _ -> None
;;

let to_dyn = function
  | Dune_package t -> Dune_package.to_dyn t
  | Opam_package t -> Opam_package.to_dyn t
;;

let set_version_and_info t ~version ~info =
  match t with
  | Dune_package t -> Dune_package (Dune_package.set_version_and_info t ~version ~info)
  | Opam_package t -> Opam_package { t with version; info }
;;

let id = function
  | Dune_package t -> Dune_package.id t
  | Opam_package t -> t.id
;;

let allow_empty = function
  | Dune_package t -> Dune_package.allow_empty t
  | Opam_package _ -> false
;;

let opam_package = function
  | Dune_package _ -> None
  | Opam_package t -> Some t

(* let depends v *) 
