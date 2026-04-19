open Stdune
module Format = Stdlib.Format

type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Template.t

let atom_or_quoted_string s =
  if Atom.is_valid s then Atom (Atom.of_string s) else Quoted_string s
;;

let atom s = Atom (Atom.of_string s)

let repr =
  Repr.fix (fun repr ->
    Repr.variant
      "dune-sexp"
      [ Repr.case "Atom" Atom.repr ~proj:(function
          | Atom atom -> Some atom
          | Quoted_string _ | List _ | Template _ -> None)
      ; Repr.case "Quoted_string" Repr.string ~proj:(function
          | Quoted_string string -> Some string
          | Atom _ | List _ | Template _ -> None)
      ; Repr.case "List" (Repr.list repr) ~proj:(function
          | List list -> Some list
          | Atom _ | Quoted_string _ | Template _ -> None)
      ; Repr.case "Template" Template.repr ~proj:(function
          | Template template -> Some template
          | Atom _ | Quoted_string _ | List _ -> None)
      ])
;;

let rec to_string t =
  match t with
  | Atom (A s) -> s
  | Quoted_string s -> Escape.quoted s
  | List l -> Printf.sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")
  | Template t -> Template.to_string t
;;

let rec pp = function
  | Atom (A s) -> Pp.verbatim s
  | Quoted_string s -> Pp.verbatim (Escape.quoted s)
  | List [] -> Pp.verbatim "()"
  | List l ->
    let open Pp.O in
    Pp.box
      ~indent:1
      (Pp.char '(' ++ Pp.hvbox (Pp.concat_map l ~sep:Pp.space ~f:pp) ++ Pp.char ')')
  | Template t -> Template.pp t
;;

let rec to_dyn =
  let open Dyn in
  function
  | Atom (A a) -> string a
  | List s -> List (List.map s ~f:to_dyn)
  | Quoted_string s -> string s
  | Template t -> variant "template" [ string (Template.to_string t) ]
;;

let rec to_sexp = function
  | Atom (A s) -> Sexp.Atom s
  | List s -> List (List.map ~f:to_sexp s)
  | Quoted_string s -> List [ Atom "quoted"; Atom s ]
  | Template ({ quoted; parts = _; loc = _; _ } as t) ->
    List [ Atom "template"; Atom (Bool.to_string quoted); Atom (Template.to_string t) ]
;;
