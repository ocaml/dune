module type S = Set_intf.S

module Make (Key : Map_intf.Key) (M : Map_intf.S with type key = Key.t) = struct
  include Stdlib.MoreLabels.Set.Make (struct
    type t = Key.t

    let compare x y = Ordering.to_int (Key.compare x y)
  end)

  type 'a map = 'a M.t

  let to_list = elements

  let mem t x = mem x t

  let add t x = add x t

  let remove t x = remove x t

  let compare a b = Ordering.of_int (compare a b)

  let is_subset t ~of_ = subset t of_

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

  let of_keys = M.foldi ~init:empty ~f:(fun k _ acc -> add acc k)

  let to_map = fold ~init:M.empty ~f:(fun k acc -> M.set acc k ())

  let of_list_map xs ~f =
    (* We don't [fold_left] & [add] over [xs] because [of_list] has a
       specialized implementation *)
    List.map xs ~f |> of_list
end
