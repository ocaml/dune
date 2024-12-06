module type S = Map_intf.S
module type Key = Map_intf.Key

module Make (Key : Key) : S with type key = Key.t = struct
  include MoreLabels.Map.Make (struct
      type t = Key.t

      let compare a b = Ordering.to_int (Key.compare a b)
    end)

  let find key t = find_opt t key
  let mem t k = mem k t
  let set t k v = add ~key:k ~data:v t
  let update t k ~f = update ~key:k ~f t

  let add_exn t key v =
    update t key ~f:(function
      | None -> Some v
      | Some _ ->
        Code_error.raise "Map.add_exn: key already exists" [ "key", Key.to_dyn key ])
  ;;

  let add (type e) (t : e t) key v =
    let module M = struct
      exception Found of e
    end
    in
    try
      Result.Ok
        (update t key ~f:(function
           | None -> Some v
           | Some e -> raise_notrace (M.Found e)))
    with
    | M.Found e -> Error e
  ;;

  let remove t k = remove k t

  let add_multi t key x =
    let l = Option.value (find t key) ~default:[] in
    set t key (x :: l)
  ;;

  let merge a b ~f = merge a b ~f
  let union a b ~f = union a b ~f

  let union_all maps ~f =
    match maps with
    | [] -> empty
    | init :: maps -> List.fold_left maps ~init ~f:(fun acc map -> union acc map ~f)
  ;;

  let union_exn a b =
    union a b ~f:(fun key _ _ ->
      Code_error.raise
        "Map.union_exn: a key appears in both maps"
        [ "key", Key.to_dyn key ])
  ;;

  let compare a b ~compare:f =
    compare a b ~cmp:(fun a b -> Ordering.to_int (f a b)) |> Ordering.of_int
  ;;

  let equal a b ~equal:f = equal a b ~cmp:f
  let iteri t ~f = iter t ~f:(fun ~key ~data -> f key data)
  let iter t ~f = iteri t ~f:(fun _ x -> f x)

  let iter2 a b ~f =
    ignore
      (merge a b ~f:(fun key a b ->
         f key a b;
         None)
       : _ t)
  ;;

  let foldi t ~init ~f = fold t ~init ~f:(fun ~key ~data acc -> f key data acc)
  let fold t ~init ~f = foldi t ~init ~f:(fun _ x acc -> f x acc)
  let for_alli t ~f = for_all t ~f
  let for_all t ~f = for_alli t ~f:(fun _ x -> f x)
  let existsi t ~f = exists t ~f
  let exists t ~f = existsi t ~f:(fun _ x -> f x)
  let filteri t ~f = filter t ~f
  let filter t ~f = filteri t ~f:(fun _ x -> f x)
  let partitioni t ~f = partition t ~f
  let partition t ~f = partitioni t ~f:(fun _ x -> f x)

  let partition_map t ~f =
    foldi t ~init:(empty, empty) ~f:(fun i x (l, r) ->
      match f x with
      | Either.Left e -> set l i e, r
      | Right e -> l, set r i e)
  ;;

  let to_list = bindings
  let to_list_map t ~f = foldi t ~init:[] ~f:(fun k v acc -> f k v :: acc) |> List.rev

  let of_list =
    let rec loop acc = function
      | [] -> Result.Ok acc
      | (k, v) :: l ->
        (match find acc k with
         | None -> loop (set acc k v) l
         | Some v_old -> Error (k, v_old, v))
    in
    fun l -> loop empty l
  ;;

  let of_list_map =
    let rec loop f acc = function
      | [] -> Result.Ok acc
      | x :: l ->
        let k, v = f x in
        if mem acc k then Error k else loop f (set acc k v) l
    in
    fun l ~f ->
      match loop f empty l with
      | Result.Ok _ as x -> x
      | Error k ->
        (match
           List.filter l ~f:(fun x ->
             match Key.compare (fst (f x)) k with
             | Eq -> true
             | _ -> false)
         with
         | x :: y :: _ -> Error (k, x, y)
         | _ -> assert false)
  ;;

  let of_list_map_exn t ~f =
    match of_list_map t ~f with
    | Result.Ok x -> x
    | Error (key, _, _) ->
      Code_error.raise "Map.of_list_map_exn" [ "key", Key.to_dyn key ]
  ;;

  let of_list_exn l =
    match of_list l with
    | Result.Ok x -> x
    | Error (key, _, _) -> Code_error.raise "Map.of_list_exn" [ "key", Key.to_dyn key ]
  ;;

  let of_list_reduce l ~f =
    List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
      match find acc key with
      | None -> set acc key data
      | Some x -> set acc key (f x data))
  ;;

  let of_list_fold l ~init ~f =
    List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
      let x = Option.value (find acc key) ~default:init in
      set acc key (f x data))
  ;;

  let of_list_reducei l ~f =
    List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
      match find acc key with
      | None -> set acc key data
      | Some x -> set acc key (f key x data))
  ;;

  let of_list_multi l =
    List.fold_left (List.rev l) ~init:empty ~f:(fun acc (key, data) ->
      add_multi acc key data)
  ;;

  let of_list_unit =
    let rec loop acc = function
      | [] -> acc
      | k :: l -> loop (set acc k ()) l
    in
    fun l -> loop empty l
  ;;

  let keys t = foldi t ~init:[] ~f:(fun k _ l -> k :: l) |> List.rev
  let values t = foldi t ~init:[] ~f:(fun _ v l -> v :: l) |> List.rev

  let find_exn t key =
    match find_opt key t with
    | Some v -> v
    | None ->
      Code_error.raise
        "Map.find_exn: failed to find key"
        [ "key", Key.to_dyn key; "keys", Dyn.list Key.to_dyn (keys t) ]
  ;;

  let min_binding = min_binding_opt
  let max_binding = max_binding_opt
  let choose = choose_opt
  let split k t = split t k
  let map t ~f = map t ~f
  let mapi t ~f = mapi t ~f

  let fold_mapi t ~init ~f =
    let acc = ref init in
    let result =
      mapi t ~f:(fun i x ->
        let new_acc, y = f i !acc x in
        acc := new_acc;
        y)
    in
    !acc, result
  ;;

  let filter_mapi t ~f =
    merge t empty ~f:(fun key data _always_none ->
      match data with
      | None -> assert false
      | Some data -> f key data)
  ;;

  let filter_map t ~f = filter_mapi t ~f:(fun _ x -> f x)
  let filter_opt t = filter_map t ~f:Fun.id

  let superpose =
    let f _ x _ = Some x in
    fun a b -> union a b ~f
  ;;

  let is_subset t ~of_ ~f =
    let not_subset () = raise_notrace Exit in
    match
      merge t of_ ~f:(fun _dir t of_ ->
        match t with
        | None -> None
        | Some t ->
          (match of_ with
           | None -> not_subset ()
           | Some of_ -> if f t ~of_ then None else not_subset ()))
    with
    | (_ : _ t) -> true
    | exception Exit -> false
  ;;

  exception Found of Key.t

  let find_key t ~f =
    match iteri t ~f:(fun key _ -> if f key then raise_notrace (Found key) else ()) with
    | () -> None
    | exception Found e -> Some e
  ;;

  let to_dyn f t = Dyn.Map (to_list t |> List.map ~f:(fun (k, v) -> Key.to_dyn k, f v))
  let to_seq = to_seq

  module Multi = struct
    type nonrec 'a t = 'a list t

    let rev_union m1 m2 = union m1 m2 ~f:(fun _ l1 l2 -> Some (List.rev_append l1 l2))

    let cons t k x =
      update t k ~f:(function
        | None -> Some [ x ]
        | Some xs -> Some (x :: xs))
    ;;

    let find t k = Option.value (find t k) ~default:[]

    let add_all t k = function
      | [] -> t
      | entries ->
        update t k ~f:(fun v ->
          Some
            (match v with
             | None -> entries
             | Some x -> List.append x entries))
    ;;

    let find_elt : type a. a t -> f:(a -> bool) -> (key * a) option =
      fun m ~f ->
      let exception Found of (key * a) in
      try
        let check_found k e = if f e then raise_notrace (Found (k, e)) in
        iteri ~f:(fun k -> List.iter ~f:(check_found k)) m;
        None
      with
      | Found p -> Some p
    ;;

    let to_flat_list t = fold t ~init:[] ~f:List.rev_append
    let map t ~f = map t ~f:(fun l -> List.map ~f l)
    let parent_equal = equal

    let equal t t' ~equal =
      parent_equal
        ~equal:(fun l l' -> Result.value ~default:false @@ List.for_all2 ~f:equal l l')
        t
        t'
    ;;

    let to_dyn a_to_dyn t = to_dyn (Dyn.list a_to_dyn) t
  end
end
