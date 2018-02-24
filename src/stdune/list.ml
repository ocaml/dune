type 'a t = 'a list

include ListLabels

let is_empty = function
  | [] -> true
  | _  -> false

let rec filter_map l ~f =
  match l with
  | [] -> []
  | x :: l ->
    match f x with
    | None -> filter_map l ~f
    | Some x -> x :: filter_map l ~f

let filteri l ~f =
  let rec filteri l i =
    match l with
    | [] -> []
    | x :: l ->
      let i' = succ i in
      if f i x
      then x :: filteri l i'
      else filteri l i'
  in
  filteri l 0

let concat_map l ~f = concat (map l ~f)

let rev_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> (accl, accr)
    | x :: l ->
      match (f x : (_, _) Either.t) with
      | Left  y -> loop l (y :: accl) accr ~f
      | Right y -> loop l accl (y :: accr) ~f
  in
  fun l ~f -> loop l [] [] ~f

let partition_map l ~f =
  let l, r = rev_partition_map l ~f in
  (rev l, rev r)

let rec find_map l ~f =
  match l with
  | [] -> None
  | x :: l ->
    match f x with
    | None -> find_map l ~f
    | Some _ as res -> res

let rec find l ~f =
  match l with
  | [] -> None
  | x :: l -> if f x then Some x else find l ~f

let max_length l ~length =
  fold_left l ~init:0 ~f:(fun acc x ->
    max acc (length x))

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last xs
