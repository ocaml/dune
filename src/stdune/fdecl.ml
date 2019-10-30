type 'a state =
  | Unset
  | Set of 'a
  | Default of 'a

type 'a t =
  { mutable state : 'a state
  ; to_dyn : 'a -> Dyn.t
  }

let create ?default to_dyn =
  let state =
    match default with
    | None -> Unset
    | Some s -> Default s
  in
  { state ; to_dyn }

let set t new_ =
  match t.state with
  | Default _
  | Unset -> t.state <- Set new_
  | Set old ->
    Code_error.raise "Fdecl.set: already set"
      [ ("old", t.to_dyn old); ("new_", t.to_dyn new_) ]

let reset t x = t.state <- Set x

let get t =
  match t.state with
  | Unset -> Code_error.raise "Fdecl.get: not set" []
  | Default x ->
    t.state <- Set x;
    x
  | Set x -> x

let peek t =
  match t.state with
  | Default d ->
    t.state <- Set d;
    Some d
  | Unset -> None
  | Set x -> Some x
