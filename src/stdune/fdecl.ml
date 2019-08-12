type 'a state =
  | Unset
  | Set of 'a

type 'a t = { mutable state : 'a state }

let create () = { state = Unset }

let set t x =
  match t.state with
  | Unset ->
      t.state <- Set x
  | Set _ ->
      Code_error.raise "Fdecl.set: already set" []

let get t =
  match t.state with
  | Unset ->
      Code_error.raise "Fdecl.get: not set" []
  | Set x ->
      x
