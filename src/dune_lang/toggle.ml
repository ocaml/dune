open Stdune
open Dune_sexp
open Dune_config
include Config.Toggle

let enabled : t -> bool = function
  | `Enabled -> true
  | `Disabled -> false
;;

let of_bool = function
  | true -> `Enabled
  | false -> `Disabled
;;

let all = [ "enable", `Enabled; "disable", `Disabled ]

let encode t =
  let open Encoder in
  let v =
    List.find_map all ~f:(fun (k, v) -> Option.some_if (v = t) k) |> Option.value_exn
  in
  string v
;;

let decode : t Decoder.t =
  Decoder.(map_validate string) ~f:(fun s ->
    match List.assoc all s with
    | Some v -> Ok v
    | None -> Error (User_error.make [ Pp.text "must be 'disable' or 'enable'" ]))
;;

let field ?check name =
  let open Decoder in
  let decode =
    match check with
    | None -> decode
    | Some check -> check >>> decode
  in
  field_o name decode
;;
