open Stdune
module Stringlike = Dune_util.Stringlike

module type Stringlike = Dune_util.Stringlike

let valid_format_doc =
  Pp.text
    "Module names must be non-empty, start with a letter, and composed only of the \
     following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'."
;;

module Modulelike (S : sig
    type t

    val module_ : string
    val description : string
    val to_string : t -> string
    val make : string -> t
  end) =
Stringlike.Make (struct
    include S

    let valid_char = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> true
      | _ -> false
    ;;

    let description_of_valid_string = Some valid_format_doc

    let is_valid_first_char = function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false
    ;;

    let has_valid_first_char s = is_valid_first_char s.[0]

    let hint_valid =
      Some
        (fun name ->
          let name =
            String.filter_map name ~f:(fun c ->
              if valid_char c
              then Some c
              else (
                match c with
                | '.' | '-' -> Some '_'
                | _ -> None))
          in
          if has_valid_first_char name then name else "M" ^ name)
    ;;

    let is_valid_module_name name =
      name <> "" && has_valid_first_char name && String.for_all name ~f:valid_char
    ;;

    let of_string_opt s = if is_valid_module_name s then Some (S.make s) else None
  end)

include String

include (
  Modulelike (struct
    type t = string

    let to_string t = t
    let module_ = "Site"
    let description = "site name"
    let make s = s
  end) :
    Stringlike with type t := t)

module Infix = Comparator.Operators (String)

let dune_site_syntax =
  Dune_sexp.Syntax.create
    ~name:"dune_site"
    ~experimental:true
    ~desc:"the sites locations extension (experimental)"
    [ (0, 1), `Since (2, 8) ]
;;
