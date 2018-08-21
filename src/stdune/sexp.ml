type t = Sexp0.t =
  | Atom of string
  | List of t list

module To_sexp = struct
  type sexp = t
  type 'a t = 'a -> sexp

  let unit () = List []
  let string s = Atom s
  let int i = Atom (Int.to_string i)
  let float f = Atom (Float.to_string f)
  let bool b = Atom (Bool.to_string b)
  let pair fa fb (a, b) = List [fa a; fb b]
  let triple fa fb fc (a, b, c) = List [fa a; fb b; fc c]
  let list f l = List (List.map l ~f)
  let array f a = list f (Array.to_list a)
  let option f = function
    | None -> List []
    | Some x -> List [f x]
  let string_set set = list string (String.Set.to_list set)
  let string_map f map = list (pair string f) (String.Map.to_list map)

  let record l =
    List (List.map l ~f:(fun (n, v) -> List [Atom n; v]))

  let unknown _ = Atom "<unknown>"

  let string_hashtbl f h =
    string_map f
      (Hashtbl.foldi h ~init:String.Map.empty ~f:(fun key data acc ->
         String.Map.add acc key data))
end

let rec to_usexp = function
  | Atom s -> Usexp.atom_or_quoted_string s
  | List xs -> Usexp.List (List.map ~f:to_usexp xs)

let to_string s = Usexp.to_string ~syntax:Dune (to_usexp s)

let pp ppf t = Usexp.pp Dune ppf (to_usexp t)
