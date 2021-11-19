module Make (Key : Map.Key) = struct
  module Key = Key
  module Map = Map.Make (Key)

  module Set = struct
    type elt = Key.t

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

    let mem t x = Map.mem t x

    let add t x = Map.set t x ()

    let remove t x = Map.remove t x

    exception Not_a_subset

    let is_subset t ~of_ =
      match
        Map.merge t of_ ~f:(fun _key a b ->
            match (a, b) with
            | Some (), None -> raise_notrace Not_a_subset
            | _ -> None)
      with
      | (_ : t) -> true
      | exception Not_a_subset -> false

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
      (a, Option.is_some x, b)

    let union a b = Map.union a b ~f:(fun _k () () -> Some ())

    let diff a b =
      Map.merge a b ~f:(fun _k a b ->
          match b with
          | Some () -> None
          | None -> a)

    let inter a b =
      Map.merge a b ~f:(fun _k a b ->
          match a with
          | Some () -> b
          | None -> None)

    let union_map l ~f =
      List.fold_left ~init:empty l ~f:(fun acc x ->
          let s = f x in
          union acc s)

    let union_all l = union_map l ~f:(fun x -> x)

    exception Found of elt

    let find t ~f =
      match
        iter t ~f:(fun e ->
            if f e then
              raise_notrace (Found e)
            else
              ())
      with
      | () -> None
      | exception Found e -> Some e

    let to_dyn t = Dyn.Set (to_list t |> List.map ~f:Key.to_dyn)

    let choose_exn t =
      match choose t with
      | Some e -> e
      | None -> Code_error.raise "Set.choose_exn" [ ("t", to_dyn t) ]

    let of_keys t = Map.map t ~f:ignore

    let to_map t ~f = Map.mapi t ~f:(fun k () -> f k)

    let of_list l = List.fold_left l ~init:empty ~f:add

    let of_list_map l ~f =
      List.fold_left l ~init:empty ~f:(fun acc x -> add acc (f x))

    let to_seq t = Map.to_seq t |> Seq.map ~f:fst
  end
end
