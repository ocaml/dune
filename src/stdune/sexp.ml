module Array = ArrayLabels
module List = ListLabels
module String = StringLabels

type t = Sexp0.t =
  | Atom of string
  | List of t list

(* XXX otherwise the dependency isn't recorded by bootstrap *)
module Sexp_intf = Sexp_intf

let rec to_string = function
  | Atom s -> Escape.quote_if_needed s
  | List l ->
    Printf.sprintf "(%s)"
      (List.map ~f:to_string l
       |> String.concat ~sep:" ")

let rec pp ppf = function
  | Atom s ->
    Format.pp_print_string ppf (Escape.quote_if_needed s)
  | List [] ->
    Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp ppf first;
    List.iter rest ~f:(fun sexp ->
      Format.pp_print_space ppf ();
      pp ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()

let hash = Dune_caml.Hashtbl.hash

let string_equal (x : string) (y : string) = (=) x y

let rec equal x y =
  match x, y with
  | Atom x, Atom y -> string_equal x y
  | List x, List y -> equal_list x y
  | _, _ -> false
and equal_list xs ys = (* replicating List.equal to avoid circular deps *)
  match xs, ys with
  | [], [] -> true
  | x :: xs, y :: ys -> equal x y && equal_list xs ys
  | _, _ -> false

let compare x y = Ordering.of_int (compare x y)
