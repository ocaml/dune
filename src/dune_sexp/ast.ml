open Stdune

type t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * t list

let rec equal x y =
  match x, y with
  | Atom (loc, atom), Atom (loc', atom') -> Loc.equal loc loc' && Atom.equal atom atom'
  | Quoted_string (loc, s), Quoted_string (loc', s') ->
    Loc.equal loc loc' && String.equal s s'
  | Template t, Template t' -> Ordering.is_eq @@ Template.compare t t'
  | List (loc, xs), List (loc', xs') -> Loc.equal loc loc' && List.equal equal xs xs'
  | _, _ -> false
;;

let atom_or_quoted_string loc s =
  match T.atom_or_quoted_string s with
  | Atom a -> Atom (loc, a)
  | Quoted_string s -> Quoted_string (loc, s)
  | Template _ | List _ -> assert false
;;

let loc (Atom (loc, _) | Quoted_string (loc, _) | List (loc, _) | Template { loc; _ }) =
  loc
;;

let rec remove_locs t : T.t =
  match t with
  | Template t -> Template (Template.remove_locs t)
  | Atom (_, s) -> Atom s
  | Quoted_string (_, s) -> Quoted_string s
  | List (_, l) -> List (List.map l ~f:remove_locs)
;;

let rec add_loc (t : T.t) ~loc =
  match t with
  | Atom s -> Atom (loc, s)
  | Quoted_string s -> Quoted_string (loc, s)
  | List l -> List (loc, List.map l ~f:(add_loc ~loc))
  | Template t -> Template { t with loc }
;;
