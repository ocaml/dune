type t =
  [ `Enabled
  | `Disabled
  ]

let repr =
  Repr.variant
    "config-toggle"
    [ Repr.case0 "Enabled" ~test:(function
        | `Enabled -> true
        | `Disabled -> false)
    ; Repr.case0 "Disabled" ~test:(function
        | `Disabled -> true
        | `Enabled -> false)
    ]
;;

let all : (string * t) list = [ "enabled", `Enabled; "disabled", `Disabled ]

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let to_string t =
  List.find_map all ~f:(fun (k, v) -> if equal v t then Some k else None)
  |> Option.value_exn
;;

let of_string s =
  match List.assoc all s with
  | Some s -> Ok s
  | None -> Error (Printf.sprintf "only %S and %S are allowed" "enabled" "disabled")
;;

let to_dyn = Repr.to_dyn repr

let of_bool = function
  | true -> `Enabled
  | false -> `Disabled
;;

let enabled : t -> bool = function
  | `Enabled -> true
  | `Disabled -> false
;;
