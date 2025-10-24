open Import

type t =
  | Inside_package of Package_id.t
  | Forbidden_packages of
      { by_dir : Package_id.t Path.Source.Map.t
      ; by_id : Path.Source.t Package_id.Map.t
      }

let key : t Univ_map.Key.t = Univ_map.Key.create ~name:"package-mask" Dyn.opaque
let decode () = Decoder.get key |> Decoder.map ~f:Option.value_exn

let validate t ~loc pkg =
  match t with
  | Inside_package pkg' ->
    if Ordering.is_eq (Package_id.compare pkg pkg')
    then Ok ()
    else
      Error
        (User_error.make
           ~loc
           [ Pp.textf
               "Package %s may not be defined here"
               (Package_name.to_string pkg.name)
           ; Pp.textf
               "The only package that can be defined in this directory is %s because the \
                directory of this stanza is exclusive to this package"
               (Package_name.to_string pkg'.name)
           ])
  | Forbidden_packages { by_dir = _; by_id } ->
    (match Package_id.Map.find by_id pkg with
     | None -> Ok ()
     | Some dir ->
       Error
         (User_error.make
            ~loc
            [ Pp.textf
                "Package %s may not be defined here"
                (Package_name.to_string pkg.name)
            ; Pp.textf
                "That package must exist in %s"
                (Path.Source.to_string_maybe_quoted dir)
            ]))
;;

let package_env ~dir ~packages:by_dir =
  if Path.Source.Map.is_empty by_dir
  then Forbidden_packages { by_dir; by_id = Package_id.Map.empty }
  else (
    let rec find dir =
      match Path.Source.Map.find by_dir dir with
      | Some s -> Some s
      | None ->
        (match Path.Source.parent dir with
         | None -> None
         | Some s -> find s)
    in
    match find dir with
    | Some p -> Inside_package p
    | None ->
      let by_id =
        Path.Source.Map.to_list by_dir
        |> Package_id.Map.of_list_map_exn ~f:(fun (dir, id) -> id, dir)
      in
      Forbidden_packages { by_id; by_dir })
;;

let decode_pkg : Package_id.t option Decoder.t =
  let open Decoder in
  Decoder.get key
  >>| function
  | None -> None
  | Some (Inside_package id) -> Some id
  | Some (Forbidden_packages _) -> None
;;
