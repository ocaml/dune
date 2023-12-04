module type S = Set_intf.S

module Make (Key : Map_intf.Key) (M : Map_intf.S with type key = Key.t) = struct
  include Stdlib.MoreLabels.Set.Make (struct
      type t = Key.t

      let compare x y = Ordering.to_int (Key.compare x y)
    end)

  type 'a map = 'a M.t

  let to_list = elements

  let to_list_map t ~f =
    (* optimize if we get a right fold *)
    fold t ~init:[] ~f:(fun a acc -> f a :: acc) |> List.rev
  ;;

  let mem t x = mem x t
  let add t x = add x t
  let remove t x = remove x t
  let compare a b = Ordering.of_int (compare a b)
  let is_subset t ~of_ = subset t of_
  let are_disjoint a b = not (exists a ~f:(mem b))
  let iter t ~f = iter t ~f
  let map t ~f = map t ~f
  let fold t ~init ~f = fold t ~init ~f
  let for_all t ~f = for_all t ~f
  let exists t ~f = exists t ~f
  let filter t ~f = filter t ~f
  let partition t ~f = partition t ~f
  let min_elt = min_elt_opt
  let max_elt = max_elt_opt
  let choose = choose_opt
  let split x t = split t x

  let union_map l ~f =
    List.fold_left ~init:empty l ~f:(fun acc x ->
      let s = f x in
      union acc s)
  ;;

  let union_all l = union_map l ~f:Fun.id

  exception Found of elt

  let find t ~f =
    match iter t ~f:(fun e -> if f e then raise_notrace (Found e) else ()) with
    | () -> None
    | exception Found e -> Some e
  ;;

  let to_dyn t = Dyn.Set (to_list t |> List.map ~f:Key.to_dyn)

  let choose_exn t =
    match choose t with
    | Some e -> e
    | None -> Code_error.raise "Set.choose_exn" [ "t", to_dyn t ]
  ;;

  let of_keys = M.foldi ~init:empty ~f:(fun k _ acc -> add acc k)
  let to_map t ~f = fold t ~init:M.empty ~f:(fun k acc -> M.set acc k (f k))

  let of_list_map xs ~f =
    (* We don't [fold_left] & [add] over [xs] because [of_list] has a
       specialized implementation *)
    List.map xs ~f |> of_list
  ;;
end

module Of_map (Key : Map_intf.Key) (Map : Map_intf.S with type key = Key.t) = struct
  type elt = Key.t
  type 'a map = 'a Map.t
  type t = unit Map.t

  let empty = Map.empty
  let is_empty = Map.is_empty
  let singleton x = Map.singleton x ()
  let cardinal = Map.cardinal
  let equal a b = Map.equal a b ~equal:(fun () () -> true)
  let compare a b = Map.compare a b ~compare:(fun () () -> Eq)
  let to_list = Map.keys

  let to_list_map t ~f =
    (* optimize if we get a right fold *)
    Map.to_list_map t ~f:(fun a () -> f a)
  ;;

  let mem t x = Map.mem t x
  let add t x = Map.set t x ()
  let remove t x = Map.remove t x

  exception Not_a_subset

  let is_subset t ~of_ =
    match
      Map.merge t of_ ~f:(fun _key a b ->
        match a, b with
        | Some (), None -> raise_notrace Not_a_subset
        | _ -> None)
    with
    | (_ : t) -> true
    | exception Not_a_subset -> false
  ;;

  let are_disjoint a b = not (Map.existsi a ~f:(fun k () -> mem b k))
  let iter t ~f = Map.iteri t ~f:(fun k () -> f k)
  let fold t ~init ~f = Map.foldi t ~init ~f:(fun k () acc -> f k acc)
  let map t ~f = fold t ~init:empty ~f:(fun x acc -> add acc (f x))
  let for_all t ~f = Map.for_alli t ~f:(fun k () -> f k)
  let exists t ~f = Map.existsi t ~f:(fun k () -> f k)
  let filter t ~f = Map.filteri t ~f:(fun k () -> f k)
  let partition t ~f = Map.partitioni t ~f:(fun k () -> f k)
  let min_elt t = Map.min_binding t |> Option.map ~f:fst
  let max_elt t = Map.max_binding t |> Option.map ~f:fst
  let choose t = Map.choose t |> Option.map ~f:fst

  let split t x =
    let a, x, b = Map.split t x in
    a, Option.is_some x, b
  ;;

  let union a b = Map.union a b ~f:(fun _k () () -> Some ())

  let diff a b =
    Map.merge a b ~f:(fun _k a b ->
      match b with
      | Some () -> None
      | None -> a)
  ;;

  let inter a b =
    Map.merge a b ~f:(fun _k a b ->
      match a with
      | Some () -> b
      | None -> None)
  ;;

  let union_map l ~f =
    List.fold_left ~init:empty l ~f:(fun acc x ->
      let s = f x in
      union acc s)
  ;;

  let union_all l = union_map l ~f:Fun.id

  exception Found of elt

  let find t ~f =
    match iter t ~f:(fun e -> if f e then raise_notrace (Found e) else ()) with
    | () -> None
    | exception Found e -> Some e
  ;;

  let to_dyn t = Dyn.Set (to_list t |> List.map ~f:Key.to_dyn)

  let choose_exn t =
    match choose t with
    | Some e -> e
    | None -> Code_error.raise "Set.choose_exn" [ "t", to_dyn t ]
  ;;

  let of_keys t = Map.map t ~f:ignore
  let to_map t ~f = Map.mapi t ~f:(fun k () -> f k)
  let of_list l = List.fold_left l ~init:empty ~f:add
  let of_list_map l ~f = List.fold_left l ~init:empty ~f:(fun acc x -> add acc (f x))
  let to_seq t = Map.to_seq t |> Seq.map ~f:fst
end
