open Stdune

module Reason = struct
  module T = struct
    type t =
      | Requested
      | Timeout
      | Signal of Signal.t

    let to_dyn t =
      match t with
      | Requested -> Dyn.Variant ("Requested", [])
      | Timeout -> Dyn.Variant ("Timeout", [])
      | Signal signal -> Dyn.Variant ("Signal", [ Signal.to_dyn signal ])
    ;;

    let compare a b =
      match a, b with
      | Requested, Requested -> Eq
      | Requested, _ -> Lt
      | _, Requested -> Gt
      | Timeout, Timeout -> Eq
      | Timeout, _ -> Lt
      | _, Timeout -> Gt
      | Signal a, Signal b -> Signal.compare a b
    ;;
  end

  include T
  include Comparable.Make (T)
end

exception E of Reason.t

let () =
  Printexc.register_printer (function
    | E Requested -> Some "shutdown: requested"
    | E Timeout -> Some "shutdown: timeout"
    | E (Signal s) -> Some (sprintf "shutdown: signal %s received" (Signal.name s))
    | _ -> None)
;;
