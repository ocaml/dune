open Import

type t =
  { modules : string list
  ; loc : Loc.t
  }

type Stanza.t += T of t
