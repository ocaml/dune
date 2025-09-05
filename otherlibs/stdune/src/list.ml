include ListLabels

type 'a t = 'a list

let map ~f t = rev (rev_map ~f t)

let is_empty = function
  | [] -> true
  | _ -> false
;;

let is_non_empty = function
  | [] -> false
  | _ -> true
;;

let rev_filter_map l ~f =
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
      (match f x with
       | None -> loop acc xs
       | Some x -> loop (x :: acc) xs)
  in
  loop [] l
;;

let filter_map l ~f = rev (rev_filter_map l ~f)
let filter_opt l = filter_map ~f:Fun.id l

let filteri l ~f =
  let rec filteri l i =
    match l with
    | [] -> []
    | x :: l ->
      let i' = succ i in
      if f i x then x :: filteri l i' else filteri l i'
  in
  filteri l 0
;;

let rev_concat =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (rev_append x acc) xs
  in
  fun t -> loop [] t
;;

let rev_concat_map t ~f =
  let rec aux f acc = function
    | [] -> acc
    | x :: l ->
      let xs = f x in
      aux f (rev_append xs acc) l
  in
  aux f [] t
;;

let concat_map t ~f = rev (rev_concat_map t ~f)

let rev_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> accl, accr
    | x :: l ->
      (match (f x : (_, _) Either.t) with
       | Left y -> loop l (y :: accl) accr ~f
       | Right y -> loop l accl (y :: accr) ~f)
  in
  fun l ~f -> loop l [] [] ~f
;;

let partition_map l ~f =
  let l, r = rev_partition_map l ~f in
  rev l, rev r
;;

type ('a, 'b) skip_or_either =
  | Skip
  | Left of 'a
  | Right of 'b

let rev_filter_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> accl, accr
    | x :: l ->
      (match f x with
       | Skip -> loop l accl accr ~f
       | Left y -> loop l (y :: accl) accr ~f
       | Right y -> loop l accl (y :: accr) ~f)
  in
  fun l ~f -> loop l [] [] ~f
;;

let filter_partition_map l ~f =
  let l, r = rev_filter_partition_map l ~f in
  rev l, rev r
;;

let rec find_map l ~f =
  match l with
  | [] -> None
  | x :: l ->
    (match f x with
     | None -> find_map l ~f
     | Some _ as res -> res)
;;

let findi l ~f =
  let rec findi acc l ~f =
    match l with
    | [] -> None
    | x :: l -> if f x then Some (x, acc) else findi (acc + 1) l ~f
  in
  findi 0 l ~f
;;

let rec find l ~f =
  match l with
  | [] -> None
  | x :: l -> if f x then Some x else find l ~f
;;

let find_exn l ~f =
  match find l ~f with
  | Some x -> x
  | None -> Code_error.raise "List.find_exn" []
;;

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs
;;

let destruct_last =
  let rec loop acc = function
    | [] -> None
    | [ x ] -> Some (rev acc, x)
    | x :: xs -> loop (x :: acc) xs
  in
  fun xs -> loop [] xs
;;

let remove_last_exn t =
  match destruct_last t with
  | Some (t, _) -> t
  | None -> Code_error.raise "remove_last_exn: empty list" []
;;

let sort t ~compare = sort t ~cmp:(fun a b -> Ordering.to_int (compare a b))
let stable_sort t ~compare = stable_sort t ~cmp:(fun a b -> Ordering.to_int (compare a b))

let sort_uniq t ~compare =
  Stdlib.List.sort_uniq (fun a b -> Ordering.to_int (compare a b)) t
;;

let rec compare a b ~compare:f : Ordering.t =
  match a, b with
  | [], [] -> Eq
  | [], _ :: _ -> Lt
  | _ :: _, [] -> Gt
  | x :: a, y :: b ->
    (match (f x y : Ordering.t) with
     | Eq -> compare a b ~compare:f
     | ne -> ne)
;;

let rec assoc t x =
  match t with
  | [] -> None
  | (k, v) :: t -> if x = k then Some v else assoc t x
;;

let singleton x = [ x ]

let rec nth t i =
  match t, i with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: xs, i -> nth xs (i - 1)
;;

let physically_equal = Stdlib.( == )

let init =
  let rec loop acc i n f = if i = n then rev acc else loop (f i :: acc) (i + 1) n f in
  fun n ~f -> loop [] 0 n f
;;

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x
;;

let rec equal eq xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal eq xs ys
  | _, _ -> false
;;

let hash f xs = Stdlib.Hashtbl.hash (map ~f xs)
let cons x xs = x :: xs

(* copy&paste from [base] *)
let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let unzip l = fold_right ~init:([], []) ~f:(fun (x, y) (xs, ys) -> x :: xs, y :: ys) l

let rec for_all2 x y ~f =
  match x, y with
  | [], [] -> Ok true
  | x :: xs, y :: ys -> if f x y then for_all2 xs ys ~f else Ok false
  | _, _ -> Error `Length_mismatch
;;

let reduce xs ~f =
  match xs with
  | [] -> None
  | init :: xs -> Some (fold_left xs ~init ~f)
;;

let min xs ~f = reduce xs ~f:(Ordering.min f)
let max xs ~f = reduce xs ~f:(Ordering.max f)
let mem t a ~equal = exists t ~f:(equal a)

(* copy&paste from [base] *)
let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> rev acc, t
  in
  loop [] xs
;;

let truncate ~max_length xs =
  let rec loop acc length = function
    | [] -> `Not_truncated (rev acc)
    | _ :: _ when length >= max_length -> `Truncated (rev acc)
    | hd :: tl -> loop (hd :: acc) (length + 1) tl
  in
  loop [] 0 xs
;;

let intersperse xs ~sep =
  let rec loop acc = function
    | [] -> rev acc
    | [ x ] -> rev (x :: acc)
    | x :: xs -> loop (sep :: x :: acc) xs
  in
  loop [] xs
;;

let rec partition_three xs ~f =
  match xs with
  | [] -> [], [], []
  | first :: rest ->
    let xs, ys, zs = partition_three ~f rest in
    (match f first with
     | `Left x -> x :: xs, ys, zs
     | `Middle y -> xs, y :: ys, zs
     | `Right z -> xs, ys, z :: zs)
;;
