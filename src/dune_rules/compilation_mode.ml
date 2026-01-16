open Import

type t =
  | Ocaml
  | Melange

type modes =
  { modes : t list
  ; for_merlin : t
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

let of_lib_mode = function
  | Lib_mode.Ocaml _ -> Ocaml
  | Melange -> Melange
;;

let of_mode_set (modes : Lib_mode.Map.Set.t) =
  match modes.ocaml.byte, modes.ocaml.native, modes.melange with
  | false, false, true -> { modes = [ Melange ]; for_merlin = Melange }
  | true, _, false | _, true, false -> { modes = [ Ocaml ]; for_merlin = Ocaml }
  | true, _, true | _, true, true -> { modes = [ Ocaml; Melange ]; for_merlin = Ocaml }
  | false, false, false -> { modes = []; for_merlin = Ocaml }
;;

module By_mode = struct
  type mode = t

  type nonrec 'a t =
    { ocaml : 'a
    ; melange : 'a
    }

  let just t ~for_ =
    match for_ with
    | Ocaml -> { ocaml = Some t; melange = None }
    | Melange -> { ocaml = None; melange = Some t }
  ;;

  let both t = { ocaml = t; melange = t }
  let from_fun f = { ocaml = f ~for_:Ocaml; melange = f ~for_:Melange }

  let to_list t =
    match t.ocaml, t.melange with
    | Some ocaml, Some melange -> [ Ocaml, ocaml; Melange, melange ]
    | Some ocaml, None -> [ Ocaml, ocaml ]
    | None, Some melange -> [ Melange, melange ]
    | None, None -> []
  ;;

  let of_list xs ~init =
    List.fold_left xs ~init:{ ocaml = init; melange = init } ~f:(fun acc (k, item) ->
      match k with
      | Ocaml -> { acc with ocaml = item }
      | Melange -> { acc with melange = item })
  ;;

  let map t ~f = { ocaml = f ~for_:Ocaml t.ocaml; melange = f ~for_:Melange t.melange }

  let get ~for_ t =
    match for_ with
    | Ocaml -> t.ocaml
    | Melange -> t.melange
  ;;

  let set ~for_ t x =
    match for_ with
    | Ocaml -> { t with ocaml = x }
    | Melange -> { t with melange = x }
  ;;

  let to_dyn f t =
    let open Dyn in
    record [ "ocaml", f t.ocaml; "melange", f t.melange ]
  ;;

  module Memo = struct
    open Memo.O

    let from_fun f =
      let+ ocaml = f ~for_:Ocaml
      and+ melange = f ~for_:Melange in
      { ocaml; melange }
    ;;

    let map
      :  'a option t
      -> f:(for_:mode -> 'a option -> 'b option Memo.t)
      -> 'b option t Memo.t
      =
      fun t ~f ->
      let+ y =
        let { ocaml; melange } = map t ~f in
        Memo.parallel_map
          [ Ocaml, ocaml; Melange, melange ]
          ~f:(fun (for_, x) ->
            let+ x = x in
            for_, x)
      in
      of_list y ~init:None
    ;;
  end
end
