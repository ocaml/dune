open Import
open Dune_lang.Decoder

type t =
  { name : Loc.t * Lib_name.t
  ; package : Package.t
  ; sub_dir : string option
  }

let sub_dir t = t.sub_dir
let loc t = fst t.name
let name t = snd t.name
let package t = t.package

(** if [~allow_deprecated_names] is set, then we allow the package name to be
    attached to one of the deprecated packages *)
let make ~allow_deprecated_names project ((_, s) as loc_name) =
  let pkg, rest = Lib_name.split s in
  match
    match allow_deprecated_names with
    | false -> None
    | true ->
      Dune_project.including_hidden_packages project
      |> Package.Name.Map.values
      |> List.find_map ~f:(fun package ->
        let deprecated_package_names = Package.deprecated_package_names package in
        if Package.Name.Map.mem deprecated_package_names pkg
        then Some { package; sub_dir = None; name = loc_name }
        else None)
  with
  | Some x -> Ok x
  | None ->
    Stanza_common.Pkg.resolve project pkg
    |> Result.map ~f:(fun pkg ->
      { package = pkg
      ; sub_dir = (if rest = [] then None else Some (String.concat rest ~sep:"/"))
      ; name = loc_name
      })
;;

let decode ~allow_deprecated_names =
  map_validate
    (let+ project = Dune_project.get_exn ()
     and+ loc_name = located Lib_name.decode in
     project, loc_name)
    ~f:(fun (project, loc_name) -> make ~allow_deprecated_names project loc_name)
;;
