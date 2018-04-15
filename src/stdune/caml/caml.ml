(** This library is internal to jbuilder and guarantees no API stability. *)

module Filename = Filename
module String   = String
module Char     = Char
module Result   = Result

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok    of 'a
  | Error of 'error
