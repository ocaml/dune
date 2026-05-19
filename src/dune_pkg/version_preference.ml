open Import

type t =
  | Newest
  | Oldest

let repr =
  Repr.variant
    "version-preference"
    [ Repr.case0 "newest" ~test:(function
        | Newest -> true
        | Oldest -> false)
    ; Repr.case0 "oldest" ~test:(function
        | Oldest -> true
        | Newest -> false)
    ]
;;

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let to_string = function
  | Newest -> "newest"
  | Oldest -> "oldest"
;;

let to_dyn = Repr.to_dyn repr
let default = Newest
let all = [ Newest; Oldest ]
let all_by_string = List.map all ~f:(fun t -> to_string t, t)
let decode = Decoder.enum all_by_string
