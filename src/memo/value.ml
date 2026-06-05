open! Import

(* [Error.E] (defined in memo.ml) wraps an exception together with a memoized call stack.
   To deduplicate errors we compare the underlying exceptions, but [Error] depends on the
   node graph and so cannot be referenced here. The unwrapping function is therefore
   installed by memo.ml at startup via [unwrap_exn]. *)
let unwrap_exn = ref (fun (exn : exn) -> exn)

module Exn_comparable = Comparable.Make (struct
    type t = Exn_with_backtrace.t

    let compare { Exn_with_backtrace.exn; backtrace = _ } (t : t) =
      Poly.compare (!unwrap_exn exn) (!unwrap_exn t.exn)
    ;;

    let to_dyn = Exn_with_backtrace.to_dyn
  end)

module Exn_set = Exn_comparable.Set

module Collect_errors_monoid = struct
  module T = struct
    type t =
      { exns : Exn_set.t
      ; reproducible : bool
      }

    let empty = { exns = Exn_set.empty; reproducible = true }

    let combine
          { exns = exns1; reproducible = reproducible1 }
          { exns = exns2; reproducible = reproducible2 }
      =
      { exns = Exn_set.union exns1 exns2; reproducible = reproducible1 && reproducible2 }
    ;;
  end

  include T
  include Monoid.Make (T)
end

(* Restoring or computing a value can fail when the user-supplied function raises one or
   more exceptions, recorded in the [Collect_errors_monoid.t]. A computation cancelled due
   to a dependency cycle is recorded as a non-reproducible [Error] whose exception set holds
   the corresponding cycle error. *)
type 'a t =
  | Ok of 'a
  | Error of Collect_errors_monoid.t

let get_exn t ~map_exn =
  match t with
  | Ok a -> Fiber.return a
  | Error { Collect_errors_monoid.exns; reproducible = _ } ->
    Fiber.reraise_all
      (Exn_set.to_list_map exns ~f:(fun (exn : Exn_with_backtrace.t) ->
         { exn with exn = map_exn exn.exn }))
;;
