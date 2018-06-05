open Stdune

module T = struct
  type t =
    | Paths   of Path.t list
    | Strings of string list

  let length = function
    | Paths x -> List.length x
    | Strings x -> List.length x

  let is_multivalued = function
    | Paths [_] -> false
    | Strings [_] -> false
    | _ -> true

  type context = Path.t (* For String_with_vars.Expand_to *)

  let concat = function
    | [s] -> s
    | l -> String.concat ~sep:" " l

  let string_of_path ~dir p = Path.reach ~from:dir p

  let to_string (dir: context) = function
    | Strings l -> concat l
    | Paths   l -> concat (List.map l ~f:(string_of_path ~dir))
end

include T

module Expand = String_with_vars.Expand_to(T)

let path_of_string dir s = Path.relative dir s

let to_strings dir = function
  | Strings l -> l
  | Paths   l -> List.map l ~f:(string_of_path ~dir)

let to_path dir = function
  | Strings l -> path_of_string dir (concat l)
  | Paths [p] -> p
  | Paths l ->
    path_of_string dir (concat (List.map l ~f:(string_of_path ~dir)))

module Single = struct
  let path ~dir sw ~f =
    let relative = Path.relative ~error_loc:(String_with_vars.loc sw) in
    match Expand.expand dir sw ~allow_multivalue:false ~f with
    | String s
    | Expansion (Strings [s]) -> relative dir s
    | Expansion (Paths [s]) -> Path.append dir s
    | _ -> assert false (* multivalues aren't allowed *)

  let string ~dir sw ~f =
    match Expand.expand dir sw ~allow_multivalue:false ~f with
    | String s
    | Expansion (Strings [s]) -> s
    | Expansion (Paths [s]) -> string_of_path ~dir s
    | _ -> assert false (* multivalues aren't allowed *)
end
