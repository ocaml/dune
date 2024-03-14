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

module Pp = struct
  include Pp

  (** This version of [Pp.compare] uses [Ordering.t] rather than returning an [int]. *)
  let compare ~compare x y =
    Ordering.of_int (Pp.compare (fun a b -> Ordering.to_int (compare a b)) x y)
  ;;

  let to_dyn tag_to_dyn t =
    match Pp.to_ast t with
    | Error _ -> Dyn.variant "Contains Format" [ Dyn.opaque "<error>" ]
    | Ok t ->
      let open Dyn in
      let rec to_dyn t =
        match (t : _ Pp.Ast.t) with
        | Nop -> variant "Nop" []
        | Seq (x, y) -> variant "Seq" [ to_dyn x; to_dyn y ]
        | Concat (x, y) -> variant "Concat" [ to_dyn x; list to_dyn y ]
        | Box (i, t) -> variant "Box" [ int i; to_dyn t ]
        | Vbox (i, t) -> variant "Vbox" [ int i; to_dyn t ]
        | Hbox t -> variant "Hbox" [ to_dyn t ]
        | Hvbox (i, t) -> variant "Hvbox" [ int i; to_dyn t ]
        | Hovbox (i, t) -> variant "Hovbox" [ int i; to_dyn t ]
        | Verbatim s -> variant "Verbatim" [ string s ]
        | Char c -> variant "Char" [ char c ]
        | Break (x, y) ->
          variant "Break" [ triple string int string x; triple string int string y ]
        | Newline -> variant "Newline" []
        | Text s -> variant "Text" [ string s ]
        | Tag (s, t) -> variant "Tag" [ tag_to_dyn s; to_dyn t ]
      in
      to_dyn t
  ;;
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

module Unix_error = struct
  include Dune_filesystem_stubs.Unix_error

  module Detailed = struct
    include Dune_filesystem_stubs.Unix_error.Detailed

    let to_dyn (error, syscall, arg) =
      Dyn.Record
        [ "error", String (Unix.error_message error)
        ; "syscall", String syscall
        ; "arg", String arg
        ]
    ;;

    let pp ?(prefix = "") unix_error = Pp.verbatim (prefix ^ to_string_hum unix_error)
  end
end

module File_kind = struct
  include Dune_filesystem_stubs.File_kind

  let to_dyn t = Dyn.String (to_string t)
end

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
