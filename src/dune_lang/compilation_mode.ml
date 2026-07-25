open Import

type t =
  | Ocaml
  | Melange

let equal a b =
  match a, b with
  | Ocaml, Ocaml | Melange, Melange -> true
  | Ocaml, Melange | Melange, Ocaml -> false
;;

let repr =
  Repr.variant
    "compilation-mode"
    [ Repr.case0 "Ocaml" ~test:(function
        | Ocaml -> true
        | Melange -> false)
    ; Repr.case0 "Melange" ~test:(function
        | Melange -> true
        | Ocaml -> false)
    ]
;;

let to_dyn = Repr.to_dyn repr

module Set = struct
  type t =
    { ocaml : bool
    ; melange : bool
    }

  let of_lib_mode_set (modes : Lib_mode.Map.Set.t) =
    { ocaml = modes.ocaml.byte || modes.ocaml.native; melange = modes.melange }
  ;;

  let to_list { ocaml; melange } =
    match ocaml, melange with
    | true, true -> [ Ocaml; Melange ]
    | true, false -> [ Ocaml ]
    | false, true -> [ Melange ]
    | false, false -> []
  ;;

  let for_merlin { ocaml; melange } =
    match ocaml, melange with
    | true, _ -> Ocaml
    | false, true -> Melange
    | false, false -> Ocaml
  ;;
end

let of_lib_mode = function
  | Lib_mode.Ocaml _ -> Ocaml
  | Melange -> Melange
;;

let default_sandbox = Dune_engine.Sandbox_config.no_special_requirements

module Per_mode = struct
  type nonrec 'a t =
    { ocaml : 'a
    ; melange : 'a
    }

  let both t = { ocaml = t; melange = t }

  let choose t =
    match t.ocaml, t.melange with
    | Some m, _ | None, Some m -> Some m
    | None, None -> None
  ;;

  let from_fun f = { ocaml = f ~for_:Ocaml; melange = f ~for_:Melange }

  let of_list xs ~init =
    List.fold_left xs ~init:{ ocaml = init; melange = init } ~f:(fun acc (k, item) ->
      match k with
      | Ocaml -> { acc with ocaml = item }
      | Melange -> { acc with melange = item })
  ;;

  let to_list t =
    match t.ocaml, t.melange with
    | Some ocaml, Some melange -> [ Ocaml, ocaml; Melange, melange ]
    | Some ocaml, None -> [ Ocaml, ocaml ]
    | None, Some melange -> [ Melange, melange ]
    | None, None -> []
  ;;

  let just t ~for_ = of_list ~init:None [ for_, Some t ]
  let map t ~f = { ocaml = f ~for_:Ocaml t.ocaml; melange = f ~for_:Melange t.melange }

  let get ~for_ t =
    match for_ with
    | Ocaml -> t.ocaml
    | Melange -> t.melange
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
  end
end
