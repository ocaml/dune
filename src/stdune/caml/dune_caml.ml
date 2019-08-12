(** This library is internal to dune and guarantees no API stability. *)

module Bytes = Bytes
module Filename = Filename
module String = String
module Char = Char
module Result = Dune_result.Result
module Hashtbl = MoreLabels.Hashtbl
module Lexing = Lexing
module Digest = Digest
module StringLabels = StringLabels
module ListLabels = ListLabels
module List = List
module MoreLabels = MoreLabels
module ArrayLabels = ArrayLabels

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok of 'a
  | Error of 'error
