open Import

type t =
  | Inside_package of Package_id.t
  | Forbidden_packages of Package_id.t Path.Source.Map.t

let key : t Univ_map.Key.t = Univ_map.Key.create ~name:"package-mask" Dyn.opaque

let package_env ~dir ~packages =
  if Path.Source.Map.is_empty packages
  then Forbidden_packages packages
  else (
    let rec find dir =
      match Path.Source.Map.find packages dir with
      | Some s -> Some s
      | None ->
        (match Path.Source.parent dir with
         | None -> None
         | Some s -> find s)
    in
    match find dir with
    | Some p -> Inside_package p
    | None -> Forbidden_packages packages)
;;

let decode : Package_id.t option Decoder.t =
  let open Decoder in
  Decoder.get key
  >>| function
  | None -> None
  | Some (Inside_package id) -> Some id
  | Some (Forbidden_packages _) -> None
;;
