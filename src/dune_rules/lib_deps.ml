open Import
open Dune_lang.Decoder

type t = Lib_dep.t list

let check ~loc t =
  let add kind name acc =
    match Lib_name.Map.find acc name with
    | None -> Lib_name.Map.set acc name kind
    | Some kind' -> (
      match (kind, kind') with
      | Lib_dep.Required, Required ->
        User_error.raise ~loc
          [ Pp.textf "library %S is present twice" (Lib_name.to_string name) ]
      | (Optional | Forbidden), (Optional | Forbidden) -> acc
      | Optional, Required | Required, Optional ->
        User_error.raise ~loc
          [ Pp.textf
              "library %S is present both as an optional and required \
               dependency"
              (Lib_name.to_string name)
          ]
      | Forbidden, Required | Required, Forbidden ->
        User_error.raise ~loc
          [ Pp.textf
              "library %S is present both as a forbidden and required \
               dependency"
              (Lib_name.to_string name)
          ])
  in
  ignore
    (List.fold_left t ~init:Lib_name.Map.empty ~f:(fun acc x ->
         match x with
         | Lib_dep.Re_export (_, s) | Lib_dep.Direct (_, s) ->
           add Required s acc
         | Select { choices; _ } ->
           List.fold_left choices ~init:acc
             ~f:(fun acc (c : Lib_dep.Select.Choice.t) ->
               let acc =
                 Lib_name.Set.fold c.required ~init:acc ~f:(add Optional)
               in
               Lib_name.Set.fold c.forbidden ~init:acc ~f:(add Forbidden)))
      : Lib_dep.kind Lib_name.Map.t)

let decode ~allow_re_export =
  let+ loc = loc
  and+ t = repeat (Lib_dep.decode ~allow_re_export) in
  check t ~loc;
  t

let of_pps pps = List.map pps ~f:(fun pp -> Lib_dep.direct (Loc.none, pp))
