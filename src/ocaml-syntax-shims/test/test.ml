let ( let+ ) x f = `Let (x, f)

let ( and+ ) a b = `And (a, b)

let t =
  let+ x = 1
  and+ y = 2
  and+ z = 3 in
  (x, y, z)

let () =
  match t with
  | `Let (`And (`And (1, 2), 3), f) -> assert (f ((1, 2), 3) = (1, 2, 3))
  | _ -> assert false

(* Make sure the evaluation order is the same as with OCaml >= 4.08 *)

let ( let+ ) x f = f x

let ( and+ ) a b = (a, b)

let () =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let () = Queue.add 1 q1
  and () = Queue.add 2 q1
  and () = Queue.add 3 q1 in
  let+ () = Queue.add 1 q2
  and+ () = Queue.add 2 q2
  and+ () = Queue.add 3 q2 in
  let l1 = Queue.fold (fun l x -> x :: l) [] q1 in
  let l2 = Queue.fold (fun l x -> x :: l) [] q2 in
  assert (l1 = l2)
