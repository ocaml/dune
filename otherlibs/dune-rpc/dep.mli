open Import

type t =
  | File of string
  | Directory of string
  | Glob of
      { path : string
      ; glob : string
      }

module Set : sig
  include Set.S with type elt = t

  val conv : t Conv.value
end
