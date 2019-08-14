include ListLabels

type 'a t = 'a list

let map ~f t = rev (rev_map ~f t)

let is_empty = function
  | [] -> true
  | _ -> false

let rec filter_map l ~f =
  match l with
  | [] -> []
  | x :: l -> (
    match f x with
    | None -> filter_map l ~f
    | Some x -> x :: filter_map l ~f )

let rec filter_opt l =
  match l with
  | [] -> []
  | x :: l -> (
    match x with
    | None -> filter_opt l
    | Some x -> x :: filter_opt l )

let filteri l ~f =
  let rec filteri l i =
    match l with
    | [] -> []
    | x :: l ->
      let i' = succ i in
      if f i x then
        x :: filteri l i'
      else
        filteri l i'
  in
  filteri l 0

let concat_map l ~f = concat (map l ~f)

let rev_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> (accl, accr)
    | x :: l -> (
      match (f x : (_, _) Either.t) with
      | Left y -> loop l (y :: accl) accr ~f
      | Right y -> loop l accl (y :: accr) ~f )
  in
  fun l ~f -> loop l [] [] ~f

let partition_map l ~f =
  let l, r = rev_partition_map l ~f in
  (rev l, rev r)

type ('a, 'b) skip_or_either =
  | Skip
  | Left of 'a
  | Right of 'b

let rev_filter_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> (accl, accr)
    | x :: l -> (
      match f x with
      | Skip -> loop l accl accr ~f
      | Left y -> loop l (y :: accl) accr ~f
      | Right y -> loop l accl (y :: accr) ~f )
  in
  fun l ~f -> loop l [] [] ~f

let filter_partition_map l ~f =
  let l, r = rev_filter_partition_map l ~f in
  (rev l, rev r)

let rec find_map l ~f =
  match l with
  | [] -> None
  | x :: l -> (
    match f x with
    | None -> find_map l ~f
    | Some _ as res -> res )

let rec find l ~f =
  match l with
  | [] -> None
  | x :: l ->
    if f x then
      Some x
    else
      find l ~f

let find_exn l ~f =
  match find l ~f with
  | Some x -> x
  | None -> Code_error.raise "List.find_exn" []

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs

let destruct_last =
  let rec loop acc = function
    | [] -> None
    | [ x ] -> Some (rev acc, x)
    | x :: xs -> loop (x :: acc) xs
  in
  fun xs -> loop [] xs

let sort t ~compare = sort t ~cmp:(fun a b -> Ordering.to_int (compare a b))

let stable_sort t ~compare =
  stable_sort t ~cmp:(fun a b -> Ordering.to_int (compare a b))

let sort_uniq t ~compare =
  Dune_caml.List.sort_uniq (fun a b -> Ordering.to_int (compare a b)) t

let rec compare a b ~compare:f : Ordering.t =
  match (a, b) with
  | [], [] -> Eq
  | [], _ :: _ -> Lt
  | _ :: _, [] -> Gt
  | x :: a, y :: b -> (
    match (f x y : Ordering.t) with
    | Eq -> compare a b ~compare:f
    | ne -> ne )

let rec assoc t x =
  match t with
  | [] -> None
  | (k, v) :: t ->
    if x = k then
      Some v
    else
      assoc t x

let singleton x = [ x ]

let rec nth t i =
  match (t, i) with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: xs, i -> nth xs (i - 1)

let physically_equal = (Pervasives.( == ) [@warning "-3"])

let init =
  let rec loop acc i n f =
    if i = n then
      rev acc
    else
      loop (f i :: acc) (i + 1) n f
  in
  fun n ~f -> loop [] 0 n f

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec equal eq xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal eq xs ys
  | _, _ -> false

let hash f xs = Dune_caml.Hashtbl.hash (map ~f xs)

let cons xs x = x :: xs

(* copy&paste from [base] *)
let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  (!acc, result)
