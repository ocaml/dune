module Filename = Filename
module String   = String
module Result   = Result

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok    of 'a
  | Error of 'error
