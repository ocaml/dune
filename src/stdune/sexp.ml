type t = Sexp0.t =
  | Atom of string
  | List of t list

(* XXX otherwise the dependency isn't recorded by bootstrap *)
module Sexp_intf = Sexp_intf

module To_sexp = struct
  type sexp = t
  type 'a t = 'a -> sexp

  let unit () = List []
  let string s = Atom s
  let int i = Atom (string_of_int i)
  let float f = Atom (string_of_float f)
  let bool b = Atom (string_of_bool b)
  let pair fa fb (a, b) = List [fa a; fb b]
  let triple fa fb fc (a, b, c) = List [fa a; fb b; fc c]
  let list f l = List (List.map l ~f)
  let array f a = list f (Array.to_list a)
  let option f = function
    | None -> List []
    | Some x -> List [f x]
  let string_set set = list string (String0.Set.to_list set)
  let string_map f map = list (pair string f) (String0.Map.to_list map)

  let record l =
    List (List.map l ~f:(fun (n, v) -> List [Atom n; v]))

  let unknown _ = Atom "<unknown>"

  let string_hashtbl f h =
    string_map f
      (Caml.Hashtbl.fold h ~init:String0.Map.empty ~f:(fun ~key ~data acc ->
         String0.Map.add acc key data))
end

let to_string _ = "TODO"

let pp _ppf _t = ()
