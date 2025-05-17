open Import

module Package = struct
  type t =
    { version : Package_version.t option
    ; name : Package_name.t
    ; loc : Loc.t
    }

  let decode =
    let open Decoder in
    fields
    @@
    let+ name = field "name" Package_name.decode
    and+ loc = loc
    and+ version = field_o "version" Package_version.decode in
    { name; version; loc }
  ;;
end

type t =
  { url : Loc.t * Url.t
  ; packages : Package.t list
  }

let package_map_of_list list ~loc =
  match Package_name.Map.of_list list with
  | Ok map -> map
  | Error (name, p, _) ->
    User_error.raise
      ~loc:(loc p)
      [ Pp.textf "package %S is already defined" (Package_name.to_string name) ]
;;

let common_fields =
  let open Decoder in
  let+ url = field "url" Url.decode_loc
  and+ packages = multi_field "package" Package.decode in
  { url; packages }
;;

let decode = Decoder.fields common_fields

let decode_with_name =
  let open Decoder in
  fields
  @@
  let+ t = common_fields
  and+ name = field "name" (located string) in
  name, t
;;

module Project = struct
  type nonrec t =
    { all : t list
    ; map : ((Loc.t * Url.t) * Package.t) Package_name.Map.t
    }

  let empty = { all = []; map = Package_name.Map.empty }
  let map t = t.map
  let all t = t.all
  let to_dyn = Dyn.opaque
  let hash = Poly.hash
  let equal = Poly.equal

  let decode : t Decoder.fields_parser =
    let open Decoder in
    let+ all = multi_field "pin" decode in
    let map =
      List.concat_map all ~f:(fun { url; packages } ->
        List.map packages ~f:(fun (package : Package.t) -> package.name, (url, package)))
      |> package_map_of_list ~loc:(fun (_url, (p : Package.t)) -> p.loc)
    in
    { all; map }
  ;;

  let encode _ = (* CR-rgrinberg: needed for dune init *) []
end

module Workspace = struct
  type nonrec t = ((Loc.t * Url.t) * Package.t) Package_name.Map.t String.Map.t

  let map t = t
  let empty = String.Map.empty

  let decode : t Decoder.fields_parser =
    let open Decoder in
    let+ pins = Decoder.multi_field "pin" decode_with_name in
    let pins =
      match
        String.Map.of_list_map pins ~f:(fun ((loc, name), pin) -> name, (loc, pin))
      with
      | Ok s -> s
      | Error (name, ((loc, _), _), _) ->
        User_error.raise ~loc [ Pp.textf "a pin named %S already defined" name ]
    in
    String.Map.map pins ~f:(fun (_, { url; packages }) ->
      List.map packages ~f:(fun (package : Package.t) -> package.name, (url, package))
      |> package_map_of_list ~loc:(fun (_url, (p : Package.t)) -> p.loc))
  ;;

  let equal = String.Map.equal ~equal:Poly.equal
  let to_dyn = Dyn.opaque
  let hash = Poly.hash
end
