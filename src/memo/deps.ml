open! Stdune
open Fiber.O
module Counter = Metrics.Counter

(* The array is stored reversed to avoid reversing the list in [create]. We need to be
   careful about traversing the array in the right order in the functions [to_list] and
   [changed_or_not]. *)
type 'node t = 'node array

let empty = [||]
let create ~deps_rev = Array.of_list deps_rev
let length = Array.length
let to_list t = Array.fold_left t ~init:[] ~f:(fun acc x -> x :: acc)

module Changed_or_not = struct
  type 'cycle t =
    | Unchanged
    | Changed
    | Cancelled of { dependency_cycle : 'cycle }
end

let changed_or_not t ~f =
  let rec go index =
    if index < 0
    then (
      Counter.add Metrics.Restore.edges (Array.length t);
      Fiber.return Changed_or_not.Unchanged)
    else
      f t.(index)
      >>= function
      | Changed_or_not.Unchanged -> go (index - 1)
      | (Changed | Cancelled _) as res ->
        Counter.add Metrics.Restore.edges (Array.length t - index);
        Fiber.return res
  in
  go (Array.length t - 1)
;;
