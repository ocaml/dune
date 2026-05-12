(* CR-soon Alizter: When bumping the minimal version to 4.12, remove
   [Dune_either]. [Stdlib.Either] provides this natively. *)
module Dune_either = struct
  module Either = struct
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b
  end
end

include struct
  [@@@warning "-33"]

  (* This open is unused with OCaml >= 4.12 since the stdlib defines an either
     type. *)
  open Dune_either
  open Stdlib

  type ('a, 'b) t = ('a, 'b) Either.t =
    | Left of 'a
    | Right of 'b
end

let map t ~l ~r =
  match t with
  | Left x -> l x
  | Right x -> r x
;;

let left x = Left x
let right x = Right x

let to_dyn f g = function
  | Left a -> f a
  | Right b -> g b
;;
