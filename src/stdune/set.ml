module type S = Set_intf.S

module Make(Key : Map_intf.Key)(M : Map_intf.S with type key = Key.t)
= struct
  include struct
    [@@@warning "-32"]
    (* [map] is only available since 4.04 *)
    let map ~f t =
      M.keys t
      |> List.map ~f
      |> M.of_list

    (* Since 4.05 *)
    let to_opt f t =
      match f t with
      | Some (x, _) -> Some x
      | None -> None
    let choose_opt  = to_opt M.choose
    let min_elt_opt = to_opt M.min_binding
    let max_elt_opt = to_opt M.max_binding
  end

  type elt = M.key
  type 'a map = 'a M.t
  type t = unit M.t

  let to_list = M.keys

  let is_empty = M.is_empty
  let cardinal = M.cardinal
  let equal = M.equal ~equal:(fun () () -> true)
  let empty = M.empty
  let singleton k = M.singleton k ()

  let diff = M.merge ~f:(fun _ x y ->
    match x, y with
    | None, Some ()
    | Some (), Some () -> None
    | Some (), None -> Some ()
    | None, None -> assert false)

  let inter = M.merge ~f:(fun _ x y ->
    match x, y with
    | Some (), Some () -> Some ()
    | _, _ -> None)

  let mem t x = M.mem t x
  let add t x = M.add t x ()
  let remove = M.remove
  let compare = M.compare ~compare:(fun _ _ -> Ordering.Eq)
  let is_subset t ~of_ = M.is_subset t ~of_ ~f:(fun _ ~of_:_ -> true)
  let iter t ~f = M.iteri t ~f:(fun k _ -> f k)
  let fold t ~init ~f = M.foldi t ~init ~f:(fun k () acc -> f k acc)
  let map t ~f = fold t ~init:empty ~f:(fun k acc -> add acc (f k))
  let for_all t ~f = M.for_alli t ~f:(fun k () -> f k)
  let exists t ~f = M.existsi t ~f:(fun k () -> f k)
  let filter t ~f = M.filteri t ~f:(fun k () -> f k)
  let partition t ~f = M.partitioni t ~f:(fun k () -> f k)
  let min_elt = min_elt_opt
  let max_elt = max_elt_opt
  let choose = choose_opt
  let of_list = List.fold_left ~init:empty ~f:add
  let split t e =
    let (l, e, r) = M.split t e in
    (l, Option.is_some e, r)

  let union = M.union ~f:(fun _ _ _ -> Some ())

  let union_map l ~f =
    List.fold_left ~init:empty l ~f:(fun acc x ->
      let s = f x in
      union acc s)

  let union_all l =
    union_map l ~f:(fun x -> x)

  let find t ~f = M.find_key t ~f

  let to_dyn t = Dyn.Set (to_list t |> List.map ~f:Key.to_dyn)

  let choose_exn t =
    match choose t with
    | Some e -> e
    | None ->
      Code_error.raise "Set.choose_exn"
        ["t", to_dyn t]

  let of_keys = M.map ~f:(fun _ -> ())
  let to_map t = t
end
