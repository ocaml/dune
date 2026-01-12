open Import

type t =
  | Ocaml
  | Melange

type modes =
  { modes : t list
  ; merlin : t
  }

let equal a b =
  match a, b with
  | Ocaml, Ocaml | Melange, Melange -> true
  | Ocaml, Melange | Melange, Ocaml -> false
;;

let to_dyn = function
  | Ocaml -> Dyn.variant "Ocaml" []
  | Melange -> Dyn.variant "Melange" []
;;

let modes (modes : Lib_mode.Map.Set.t) =
  match modes.ocaml.byte, modes.ocaml.native, modes.melange with
  | false, false, true -> { modes = [ Melange ]; merlin = Melange }
  | true, _, false | _, true, false -> { modes = [ Ocaml ]; merlin = Ocaml }
  | true, _, true | _, true, true -> { modes = [ Ocaml; Melange ]; merlin = Ocaml }
  | false, false, false -> { modes = []; merlin = Ocaml }
;;
