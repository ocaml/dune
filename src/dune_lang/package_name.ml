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
    let of_string_opt s = if s = "" then None else Some s
  end) :
    Dune_util.Stringlike with type t := t)

module Opam_compatible = struct
  let description_of_valid_string =
    Pp.text
      "Package names can contain letters, numbers, '-', '_' and '+', and need to contain \
       at least a letter."
  ;;

  module T = struct
    type t = string

    let module_ = "Package.Name.Strict"
    let description = "opam package name"
    let to_string s = s
    let description_of_valid_string = Some description_of_valid_string

    let is_letter = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    ;;

    let is_other_valid_char = function
      | '0' .. '9' | '-' | '+' | '_' -> true
      | _ -> false
    ;;

    let is_valid_char c = is_letter c || is_other_valid_char c

    let is_valid_string s =
      let all_chars_valid = String.for_all s ~f:is_valid_char in
      let has_one_letter = String.exists s ~f:is_letter in
      all_chars_valid && has_one_letter
    ;;

    let of_string_opt s = Option.some_if (is_valid_string s) s

    let make_valid s =
      let replaced = String.map s ~f:(fun c -> if is_valid_char c then c else '_') in
      if is_valid_string replaced then replaced else "p" ^ replaced
    ;;

    let hint_valid = Some make_valid
  end

  include Dune_util.Stringlike.Make (T)

  let make_valid = T.make_valid
  let to_package_name s = s
end

let is_opam_compatible s = Option.is_some (Opam_compatible.of_string_opt (to_string s))
