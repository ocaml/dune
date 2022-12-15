open Stdune
open T

type nonrec 'a t = 'a -> t

let unit () = List []

let char c = atom_or_quoted_string (String.make 1 c)

let string = atom_or_quoted_string

let int n = Atom (Atom.of_int n)

let float f = Atom (Atom.of_float f)

let bool b = Atom (Atom.of_bool b)

let pair fa fb (a, b) = List [ fa a; fb b ]

let triple fa fb fc (a, b, c) = List [ fa a; fb b; fc c ]

let list f l = List (List.map l ~f)

let array f a = list f (Array.to_list a)

let sexp x = x

let constr s f x = pair string f (s, x)

let option f = function
  | None -> List []
  | Some x -> List [ f x ]

let record l =
  List (List.map l ~f:(fun (n, v) -> List [ Atom (Atom.of_string n); v ]))

type field =
  | Absent
  | Normal of string * T.t
  | Inlined_list of string * T.t list

let field name f ?(equal = ( = )) ?default v =
  match default with
  | None -> Normal (name, f v)
  | Some d -> if equal d v then Absent else Normal (name, f v)

let field_o name f v =
  match v with
  | None -> Absent
  | Some v -> Normal (name, f v)

let field_b name v = if v then Inlined_list (name, []) else Absent

let field_l name f l =
  match l with
  | [] -> Absent
  | _ -> Inlined_list (name, List.map l ~f)

let field_i name f x =
  match f x with
  | [] -> Absent
  | l -> Inlined_list (name, l)

let record_fields (l : field list) =
  List.filter_map l ~f:(function
    | Absent -> None
    | Normal (name, v) -> Some (List [ Atom (Atom.of_string name); v ])
    | Inlined_list (name, l) -> Some (List (Atom (Atom.of_string name) :: l)))

let unknown _ = atom "<unknown>"

let enum xs x =
  match List.find_map xs ~f:(fun (s, x') -> Option.some_if (x = x') s) with
  | None -> Code_error.raise "Encoder.enum: invalid definition" []
  | Some s -> atom_or_quoted_string s
