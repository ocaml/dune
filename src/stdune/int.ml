module T = struct
  type t = int
  let compare (a : int) b : Ordering.t =
    if a < b then
      Lt
    else if a = b then
      Eq
    else
      Gt
end

include T

module Set = Set.Make(T)
module Map = Map.Make(T)

let of_string_exn s =
  match int_of_string s with
  | exception Failure _ ->
    failwith (Printf.sprintf "of_string_exn: invalid int %S" s)
  | s -> s
