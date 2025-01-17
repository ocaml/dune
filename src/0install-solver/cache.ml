open Stdune
open Fiber.O

module Make (CacheEntry : sig
    type t

    val to_dyn : t -> Dyn.t
    val compare : t -> t -> Ordering.t
  end) =
struct
  module M = Map.Make (CacheEntry)

  type 'a snapshot = 'a M.t
  type 'a t = 'a snapshot ref

  let create () = ref M.empty

  let lookup table make key =
    match M.find !table key with
    | Some x -> Fiber.return x
    | None ->
      let* value, process = make key in
      table := M.set !table key value;
      let+ () = process () in
      value
  ;;

  let snapshot table = !table
  let get = M.find
  let get_exn = M.find_exn

  let filter_map f m =
    M.merge m M.empty ~f:(fun key ao _bo ->
      match ao with
      | Some x -> f key x
      | None -> assert false)
  ;;
end
