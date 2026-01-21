include struct
  [@@@ocaml.warning "-53"]

  [@@@alert
    unstable "The API of this library is not stable and may change without notice."]

  [@@@alert "-unstable"]
end

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
module Format = Format
module Hashtbl = Hashtbl
module Table = Table
module Int = Int
module Id = Id
module Io = Io
module Lazy = Lazy
module List = List
module Map = Map
module Option = Option
module Or_exn = Or_exn
module Ordering = Ordering
module Flock = Flock
module Terminal_signals = Terminal_signals
module Execution_env = Execution_env
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
module Pp = Pp
module Lexbuf = Lexbuf
module Scanf = Scanf
module Sys = Sys
module Pid = Pid
module Applicative = Applicative
module Json = Json
module Log = Log
module Time = Time
module Escape0 = Escape
module Debug = Debug
module Config = Config
module Metrics = Metrics
module Counter = Counter

module type Top_closure = Top_closure.Top_closure

module Top_closure = struct
  module Make = Top_closure.Make
  module Int = Make (Int.Set) (Monad.Id)
  module String = Make (String.Set) (Monad.Id)
end

module Seq = Seq
module Temp = Temp
module Queue = Queue
module Caller_id = Caller_id
module Readdir = Readdir
module Predicate = Predicate
module Bytes_unit = Bytes_unit
module Dev_null = Dev_null
module Platform = Platform
module Per_item = Per_item
module Bit_set = Bit_set
module Unix_error = Unix_error
module File_kind = File_kind

module type Per_item = Per_item_intf.S
module type Applicative = Applicative_intf.S
module type Monad = Monad_intf.S
module type Monoid = Monoid_intf.S

external reraise : exn -> _ = "%reraise"

let compare _ _ = `Use_Poly_compare

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

let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf
let printfn a = ksprintf print_endline a

module For_tests = struct
  module Compact_position = Compact_position
end
