open Import

type t =
  | Newest
  | Oldest

let equal a b =
  match a, b with
  | Newest, Newest | Oldest, Oldest -> true
  | _ -> false
;;

let to_string = function
  | Newest -> "newest"
  | Oldest -> "oldest"
;;

let to_dyn t = Dyn.variant (to_string t) []
let default = Newest
let all = [ Newest; Oldest ]
let all_by_string = List.map all ~f:(fun t -> to_string t, t)
let decode = Decoder.enum all_by_string
