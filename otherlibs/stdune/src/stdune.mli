[@@@alert unstable "The API of this library is not stable and may change without notice."]
[@@@alert "-unstable"]

module Appendable_list = Appendable_list
module Nonempty_list = Nonempty_list
module Ansi_color = Ansi_color
module Array = Array
module Bytes = Bytes
module Char = Char
module Comparator = Comparator
module Either = Either
module Exn = Exn
module Exn_with_backtrace = Exn_with_backtrace
module Filename = Filename
module Filename_set = Filename_set
module Hashtbl = Hashtbl
module Table = Table
module Int = Int
module Id = Id
module Io = Io
module List = List
module Map = Map
module Option = Option
module Or_exn = Or_exn
module Ordering = Ordering

module Pp : sig
  include module type of struct
    include Pp
  end

  (** This version of [Pp.compare] uses [Ordering.t] rather than returning an [int]. *)
  val compare : compare:('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
end

module Result = Result
module Set = Set
module Signal = Signal
module Comparable = Comparable
module Comparable_intf = Comparable_intf
module Staged = Staged
module String = String
module String_builder = String_builder
module Bool = Bool
module Sexp = Sexp
module Path = Path
module Fpath = Fpath
module Univ_map = Univ_map
module Loc = Loc
module Env = Env
module Env_path = Env_path
module Proc = Proc
module Type_eq = Type_eq
module Nothing = Nothing
module Bin = Bin
module Fdecl = Fdecl
module Unit = Unit
module Monad = Monad
module State = State
module Monoid = Monoid
module Float = Float
module Tuple = Tuple
module Poly = Poly
module Code_error = Code_error
module User_error = User_error
module User_message = User_message
module User_warning = User_warning
module Lexbuf = Lexbuf
module Scanf = Scanf
module Sys = Sys
module Pid = Pid
module Applicative = Applicative

module type Top_closure = Top_closure_intf.S

module Top_closure = Top_closure
module Seq = Seq
module Temp = Temp
module Queue = Queue
module Caller_id = Caller_id
module Dune_filesystem_stubs = Dune_filesystem_stubs
module Predicate = Predicate
module Bytes_unit = Bytes_unit
module Dev_null = Dev_null
module Platform = Platform
module Per_item = Per_item
module Bit_set = Bit_set

module type Per_item = Per_item_intf.S

module Unix_error : sig
  include module type of struct
    include Dune_filesystem_stubs.Unix_error
  end

  module Detailed : sig
    include module type of struct
      include Dune_filesystem_stubs.Unix_error.Detailed
    end

    val to_dyn : Dune_filesystem_stubs.Unix_error.t * string * string -> Dyn.t
    val pp : ?prefix:string -> t -> 'a Pp.t
  end
end

module File_kind : sig
  include module type of struct
    include Dune_filesystem_stubs.File_kind
  end

  val to_dyn : t -> Dyn.t
end

module type Applicative = Applicative_intf.S
module type Monad = Monad_intf.S
module type Monoid = Monoid_intf.S

external reraise : exn -> _ = "%reraise"
val compare : 'a -> 'b -> [> `Use_Poly_compare ]

(* The following types are re-exported here so that they are always available in
   scope *)

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt

val sprintf : ('a, unit, string) format -> 'a
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val printfn : ('a, unit, string, unit) format4 -> 'a

module For_tests : sig
  module Compact_position = Compact_position
end
