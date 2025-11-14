open Import

(* Unique identifier for a build execution.
   Allows tracking multiple concurrent builds. *)

type t = int

let counter = ref 0

let create () =
  let id = !counter in
  counter := id + 1;
  id
;;

let equal = Int.equal
let compare = Int.compare
let to_dyn t = Dyn.int t
let hash = Int.hash

module T = struct
  type nonrec t = t

  let compare = compare
  let to_dyn = to_dyn
end

module Map = Map.Make (T)
module Set = Set.Make (T) (Map)
