module Array = Stdlib.Array

let swap arr i j =
  let first, second = arr.(i), arr.(j) in
  arr.(i) <- second;
  arr.(j) <- first
;;

module T = struct
  (* CR-soon Alizter: When bumping the minimal version to 4.13, remove this
     polyfill. [Stdlib.ArrayLabels] provides it natively. *)
  let[@warning "-32"] find_opt ~f a =
    let len = Stdlib.Array.length a in
    let rec loop i =
      if i = len then None else if f a.(i) then Some a.(i) else loop (i + 1)
    in
    loop 0
  ;;

  (* overwrite with stdlib version if available *)
  include ArrayLabels

  let equal f x y =
    let open Stdlib.Array in
    let len = length x in
    if len <> length y
    then false
    else (
      try
        for i = 0 to len - 1 do
          if not (f (get x i) (get y i)) then raise_notrace Exit
        done;
        true
      with
      | Exit -> false)
  ;;

  let to_dyn f t = Dyn.array f t
  let map t ~f = map t ~f
  let fold_right t ~f ~init = fold_right t ~f ~init
  let exists t ~f = exists t ~f
end

include T

let to_list_map =
  let rec loop arr i f acc =
    if i < 0
    then acc
    else (
      let acc = f (get arr i) :: acc in
      loop arr (i - 1) f acc)
  in
  fun arr ~f -> loop arr (length arr - 1) f []
;;

let of_list_map l ~f =
  let len = List.length l in
  let l = ref l in
  init len ~f:(fun _ ->
    match !l with
    | [] -> assert false
    | x :: xs ->
      l := xs;
      f x)
;;

module Immutable = struct
  include T

  let of_array_unsafe a = a
  let of_array a = copy a
  let to_list_map t ~f = to_list_map t ~f
  let of_list_map t ~f = of_list_map t ~f
end

module Sorted = struct
  module Make (Key : Map_intf.Key) = struct
    let binary_search keys key =
      let rec loop low high =
        if low > high
        then -1
        else (
          let mid = (low + high) / 2 in
          match Key.compare key keys.(mid) with
          | Eq -> mid
          | Lt -> loop low (mid - 1)
          | Gt -> loop (mid + 1) high)
      in
      loop 0 (Stdlib.Array.length keys - 1)
    ;;

    module Set = struct
      type elt = Key.t
      type t = elt array

      let empty = [||]
      let is_empty t = Stdlib.Array.length t = 0
      let length = Stdlib.Array.length

      let mem t x =
        match binary_search t x with
        | -1 -> false
        | _ -> true
      ;;

      let fold t ~init ~f =
        let rec loop i acc = if i = length t then acc else loop (i + 1) (f t.(i) acc) in
        loop 0 init
      ;;

      let equal a b =
        a == b
        || (length a = length b
            &&
            let rec loop i =
              i = length a || (Key.compare a.(i) b.(i) = Eq && loop (i + 1))
            in
            loop 0)
      ;;

      let is_subset t ~of_ =
        t == of_
        ||
        let len_t = length t in
        let len_of = length of_ in
        let rec loop i j =
          i = len_t
          || (j < len_of
              &&
              match Key.compare t.(i) of_.(j) with
              | Eq -> loop (i + 1) (j + 1)
              | Lt -> false
              | Gt -> loop i (j + 1))
        in
        loop 0 0
      ;;

      let are_disjoint a b =
        if a == b
        then is_empty a
        else (
          let len_a = length a in
          let len_b = length b in
          let rec loop i j =
            i = len_a
            || j = len_b
            ||
            match Key.compare a.(i) b.(j) with
            | Eq -> false
            | Lt -> loop (i + 1) j
            | Gt -> loop i (j + 1)
          in
          loop 0 0)
      ;;

      let union a b =
        if a == b || is_empty b
        then a
        else if is_empty a
        then b
        else (
          let rec prepend t i acc =
            if i < 0 then acc else prepend t (i - 1) (t.(i) :: acc)
          in
          let rec loop i j acc =
            if i < 0
            then prepend b j acc
            else if j < 0
            then prepend a i acc
            else (
              match Key.compare a.(i) b.(j) with
              | Eq -> loop (i - 1) (j - 1) (a.(i) :: acc)
              | Lt -> loop i (j - 1) (b.(j) :: acc)
              | Gt -> loop (i - 1) j (a.(i) :: acc))
          in
          loop (length a - 1) (length b - 1) [] |> Stdlib.Array.of_list)
      ;;

      let diff t other =
        let len_t = length t in
        let len_other = length other in
        if are_disjoint t other
        then t
        else (
          let rec loop i j acc =
            if i < 0
            then Stdlib.Array.of_list acc
            else if j < 0
            then loop (i - 1) j (t.(i) :: acc)
            else (
              match Key.compare t.(i) other.(j) with
              | Eq -> loop (i - 1) (j - 1) acc
              | Lt -> loop i (j - 1) acc
              | Gt -> loop (i - 1) j (t.(i) :: acc))
          in
          loop (len_t - 1) (len_other - 1) [])
      ;;

      let filter t ~f =
        let rec first_removed i =
          if i = length t then None else if f t.(i) then first_removed (i + 1) else Some i
        in
        match first_removed 0 with
        | None -> t
        | Some _ ->
          let rec loop i acc =
            if i < 0
            then Stdlib.Array.of_list acc
            else (
              let acc = if f t.(i) then t.(i) :: acc else acc in
              loop (i - 1) acc)
          in
          loop (length t - 1) []
      ;;

      let to_list_map t ~f =
        let rec loop i acc = if i < 0 then acc else loop (i - 1) (f t.(i) :: acc) in
        loop (length t - 1) []
      ;;

      let to_list = Stdlib.Array.to_list

      let of_sorted_list l =
        let rec loop l acc =
          match l with
          | [] -> Stdlib.Array.of_list (Stdlib.List.rev acc)
          | x :: rest ->
            (match acc with
             | y :: _ ->
               (match Key.compare y x with
                | Lt -> loop rest (x :: acc)
                | Eq -> loop rest acc
                | Gt ->
                  Code_error.raise
                    "Array.Sorted.Set.of_sorted_list"
                    [ "previous", Key.to_dyn y; "current", Key.to_dyn x ])
             | _ -> loop rest (x :: acc))
        in
        loop l []
      ;;

      let of_list l =
        Stdlib.List.sort (fun x y -> Ordering.to_int (Key.compare x y)) l
        |> of_sorted_list
      ;;
    end

    module Map = struct
      type key = Key.t

      type 'a t =
        { keys : key array
        ; values : 'a array
        }

      let empty = { keys = [||]; values = [||] }
      let is_empty t = Stdlib.Array.length t.keys = 0

      let mem t key =
        match binary_search t.keys key with
        | -1 -> false
        | _ -> true
      ;;

      let iteri { keys; values } ~f =
        for i = 0 to Array.length keys - 1 do
          f keys.(i) values.(i)
        done
      ;;

      let find t key =
        match binary_search t.keys key with
        | -1 -> None
        | index -> Some t.values.(index)
      ;;

      let of_sorted_list_exn = function
        | [] -> empty
        | (first_key, first_value) :: rest ->
          let len = Stdlib.List.length rest + 1 in
          let keys = Stdlib.Array.make len first_key in
          let values = Stdlib.Array.make len first_value in
          let rec loop index previous_key = function
            | [] -> { keys; values }
            | (key, value) :: rest ->
              (match Key.compare previous_key key with
               | Lt -> ()
               | Eq ->
                 Code_error.raise
                   "Array.Sorted.Map.of_sorted_list_exn"
                   [ "key", Key.to_dyn key ]
               | Gt ->
                 Code_error.raise
                   "Array.Sorted.Map.of_sorted_list_exn"
                   [ "previous", Key.to_dyn previous_key; "current", Key.to_dyn key ]);
              keys.(index) <- key;
              values.(index) <- value;
              loop (index + 1) key rest
          in
          loop 1 first_key rest
      ;;

      let of_list_exn l =
        Stdlib.List.sort
          (fun (key1, _) (key2, _) -> Ordering.to_int (Key.compare key1 key2))
          l
        |> of_sorted_list_exn
      ;;

      let union_left_biased a b =
        if a == b || is_empty b
        then a
        else if is_empty a
        then b
        else (
          let rec prepend_left i acc =
            if i < 0 then acc else prepend_left (i - 1) ((a.keys.(i), a.values.(i)) :: acc)
          in
          let rec prepend_right i acc =
            if i < 0
            then acc
            else prepend_right (i - 1) ((b.keys.(i), b.values.(i)) :: acc)
          in
          let rec loop i j acc =
            if i < 0
            then prepend_right j acc
            else if j < 0
            then prepend_left i acc
            else (
              match Key.compare a.keys.(i) b.keys.(j) with
              | Eq -> loop (i - 1) (j - 1) ((a.keys.(i), a.values.(i)) :: acc)
              | Lt -> loop i (j - 1) ((b.keys.(j), b.values.(j)) :: acc)
              | Gt -> loop (i - 1) j ((a.keys.(i), a.values.(i)) :: acc))
          in
          loop (Stdlib.Array.length a.keys - 1) (Stdlib.Array.length b.keys - 1) []
          |> of_sorted_list_exn)
      ;;

      let keys t = t.keys

      let filter_mapi t ~f =
        let rec loop i acc =
          if i < 0
          then of_sorted_list_exn acc
          else (
            match f t.keys.(i) t.values.(i) with
            | None -> loop (i - 1) acc
            | Some value -> loop (i - 1) ((t.keys.(i), value) :: acc))
        in
        loop (Stdlib.Array.length t.keys - 1) []
      ;;

      let to_list_map t ~f =
        let rec loop i acc =
          if i < 0 then acc else loop (i - 1) (f t.keys.(i) t.values.(i) :: acc)
        in
        loop (Stdlib.Array.length t.keys - 1) []
      ;;

      let to_list t = to_list_map t ~f:(fun key value -> key, value)

      let equal a b ~equal =
        a == b
        || (Set.equal a.keys b.keys
            &&
            let rec loop i =
              i = Stdlib.Array.length a.values
              || (equal a.values.(i) b.values.(i) && loop (i + 1))
            in
            loop 0)
      ;;
    end
  end
end
