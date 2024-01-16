open! Stdune
open Dune_sexp

type t =
  { conditions : (Blang.t * String_with_vars.t) list
  ; loc : Loc.t
  }

let to_dyn { conditions; loc } =
  Dyn.record
    [ "conditions", Dyn.list (Dyn.pair Blang.to_dyn String_with_vars.to_dyn) conditions
    ; "loc", Loc.to_dyn loc
    ]
;;

let decode =
  let open Decoder in
  let+ loc, conditions = located (repeat (pair Blang.decode String_with_vars.decode)) in
  { loc; conditions }
;;

let equal { conditions; loc } t =
  List.equal (Tuple.T2.equal Blang.equal String_with_vars.equal) conditions t.conditions
  && Loc.equal loc t.loc
;;
