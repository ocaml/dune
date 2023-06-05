open Stdune
include String

include (
  Dune_util.Stringlike.Make (struct
    type t = string

    let to_string x = x

    let module_ = "Package.Name"

    let description = "package name"

    let description_of_valid_string = None

    let hint_valid = None

    let of_string_opt s =
      (* DUNE3 verify no dots or spaces *)
      if s = "" then None else Some s
  end) :
    Dune_util.Stringlike with type t := t)
