type 'a state =
  | Unset
  | Set of 'a

type 'a t =
  { mutable state : 'a state
  ; to_dyn : 'a -> Dyn.t
  }

let create to_dyn = { state = Unset; to_dyn }

let set t new_ =
  match t.state with
  | Unset -> t.state <- Set new_
  | Set old ->
    Code_error.raise "Fdecl.set: already set"
      [ ("old", t.to_dyn old); ("new_", t.to_dyn new_) ]

let set_idempotent ~equal t new_ =
  match t.state with
  | Unset -> t.state <- Set new_
  | Set old ->
    if not (equal new_ old) then
      Code_error.raise "Fdecl.set_idempotent: already set to a different value"
        [ ("old", t.to_dyn old); ("new_", t.to_dyn new_) ]

let get t =
  match t.state with
  | Unset -> Code_error.raise "Fdecl.get: not set" []
  | Set x -> x
