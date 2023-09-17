module Make (Key : Map.Key) : Per_item_intf.S with type key = Key.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a t =
    { map : int Map.t
    ; values : 'a Array.Immutable.t
    }

  let to_dyn f { map; values } =
    let open Dyn in
    record [ "map", Map.to_dyn int map; "values", Array.Immutable.to_dyn f values ]
  ;;

  let equal f t { values; map } =
    Array.Immutable.equal f t.values values && Map.equal ~equal:Int.equal t.map map
  ;;

  let for_all x = { map = Map.empty; values = Array.Immutable.of_array [| x |] }

  let of_mapping l ~default =
    let values = Array.Immutable.of_list (default :: List.map l ~f:snd) in
    match
      List.mapi l ~f:(fun i (keys, _) -> List.map keys ~f:(fun key -> key, i + 1))
      |> List.concat
      |> Map.of_list
    with
    | Ok map -> Ok { map; values }
    | Error (key, x, y) ->
      Error (key, Array.Immutable.get values x, Array.Immutable.get values y)
  ;;

  let get t key =
    let index =
      match Map.find t.map key with
      | None -> 0
      | Some i -> i
    in
    Array.Immutable.get t.values index
  ;;

  let map t ~f = { t with values = Array.Immutable.map t.values ~f }
  let fold t ~init ~f = Array.Immutable.fold_right t.values ~init ~f
  let exists t ~f = Array.Immutable.exists t.values ~f
  let is_constant t = Array.Immutable.length t.values = 1

  module Make_monad_traversals (M : sig
      include Monad.S

      val all : 'a t list -> 'a list t
    end) =
  struct
    let map { map; values } ~f =
      let open M.O in
      let l = Array.Immutable.to_list values in
      let+ new_values = List.map l ~f |> M.all in
      { map; values = Array.Immutable.of_list new_values }
    ;;

    let fold t ~init ~f =
      let open M.O in
      let rec loop i acc =
        if i = Array.Immutable.length t.values
        then M.return acc
        else
          let* acc = f (Array.Immutable.get t.values i) acc in
          loop (i + 1) acc
      in
      loop 0 init
    ;;
  end
end
