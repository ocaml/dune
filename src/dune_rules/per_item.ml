open Import

module Make (Key : Map.Key) : Per_item_intf.S with type key = Key.t = struct
  module Map = Map.Make (Key)

  type key = Key.t

  type 'a t =
    { map : int Map.t
    ; values : 'a array
    }

  let to_dyn f { map; values } =
    let open Dyn in
    record [ ("map", Map.to_dyn int map); ("values", array f values) ]

  let equal f t { values; map } =
    Array.equal f t.values values && Map.equal ~equal:Int.equal t.map map

  let for_all x = { map = Map.empty; values = [| x |] }

  let of_mapping l ~default =
    let values = Array.of_list (default :: List.map l ~f:snd) in
    List.mapi l ~f:(fun i (keys, _) ->
        List.map keys ~f:(fun key -> (key, i + 1)))
    |> List.concat |> Map.of_list
    |> function
    | Ok map -> Ok { map; values }
    | Error (key, x, y) -> Error (key, values.(x), values.(y))

  let get t key =
    let index =
      match Map.find t.map key with
      | None -> 0
      | Some i -> i
    in
    t.values.(index)

  let map t ~f = { t with values = Array.map t.values ~f }

  let fold t ~init ~f = Array.fold_right t.values ~init ~f

  let fold_resolve t ~init ~f =
    let open Resolve.Memo.O in
    let rec loop i acc =
      if i = Array.length t.values then Resolve.Memo.return acc
      else
        let* acc = f t.values.(i) acc in
        loop (i + 1) acc
    in
    loop 0 init

  let exists t ~f = Array.exists t.values ~f

  let is_constant t = Array.length t.values = 1

  module Make_monad_traversals (M : sig
    include Monad.S

    val all : 'a t list -> 'a list t
  end) =
  struct
    let map { map; values } ~f =
      let open M.O in
      let l = Array.to_list values in
      let+ new_values = List.map l ~f |> M.all in
      { map; values = Array.of_list new_values }
  end

  module A = Make_monad_traversals (Action_builder)

  let map_action_builder = A.map

  module R = Make_monad_traversals (Resolve.Memo)

  let map_resolve = R.map
end
