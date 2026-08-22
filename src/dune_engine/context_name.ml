open Import
include String

let default = "default"
let build_dir t = Path.Build.relative Path.Build.root t
let is_default = String.equal default

include (
  Stringlike.Make (struct
    include String

    let description_of_valid_string = None
    let hint_valid = None
    let to_string t = t
    let module_ = "Context_name"
    let description = "context name"

    let rec contains_dir_sep name i =
      if i < 0
      then false
      else (
        match String.unsafe_get name i with
        | '/' | '\\' | ':' -> true
        | _ -> contains_dir_sep name (i - 1))
    ;;

    let of_string_opt name =
      let len = String.length name in
      if
        len = 0
        || Char.equal (String.unsafe_get name 0) '.'
        || (len = 3 && String.equal name "log")
        || contains_dir_sep name (len - 1)
      then None
      else Some name
    ;;
  end) :
    Stringlike with type t := t)

let target t ~toolchain = sprintf "%s.%s" (to_string t) (to_string toolchain)
let compare = String.compare

module Infix = Comparator.Operators (String)
module Top_closure = Top_closure.Make (String.Set) (Monad.Id)
