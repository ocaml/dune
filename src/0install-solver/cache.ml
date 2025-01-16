open Fiber.O

module Make (CacheEntry : sig
    type t

    val compare : t -> t -> Ordering.t
  end) =
struct
  module M = Map.Make (struct
      include CacheEntry

      let compare x y = Ordering.to_int (CacheEntry.compare x y)
    end)

  type 'a snapshot = 'a M.t
  type 'a t = 'a snapshot ref

  let create () = ref M.empty

  let lookup table make key =
    match M.find_opt key !table with
    | Some x -> Fiber.return x
    | None ->
      let* value, process = make key in
      table := M.add key value !table;
      let+ () = process () in
      value
  ;;

  let snapshot table = !table
  let get = M.find_opt
  let get_exn = M.find

  let filter_map f m =
    M.merge
      (fun key ao _bo ->
        match ao with
        | Some x -> f key x
        | None -> assert false)
      m
      M.empty
  ;;
end
