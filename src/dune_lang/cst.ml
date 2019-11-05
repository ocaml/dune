open Stdune
module Comment = Lexer.Token.Comment

type t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * t list
  | Comment of Loc.t * Comment.t

let rec to_dyn =
  let open Dyn.Encoder in
  function
  | Atom (_, a) -> constr "Atom" [ Atom.to_dyn a ]
  | Quoted_string (_, s) -> constr "Quoted_string" [ string s ]
  | Template t -> constr "Template" [ Template.to_dyn t ]
  | List (_, l) -> constr "List" [ list to_dyn l ]
  | Comment (_, c) -> constr "Comment" [ Comment.to_dyn c ]

let loc
    ( Atom (loc, _)
    | Quoted_string (loc, _)
    | List (loc, _)
    | Template { loc; _ }
    | Comment (loc, _) ) =
  loc

let fetch_legacy_comments t ~file_contents =
  let rec loop t =
    match t with
    | Template _
    | Quoted_string _
    | Atom _
    | Comment (_, Lines _) ->
      t
    | List (loc, l) -> List (loc, List.map l ~f:loop)
    | Comment (loc, Legacy) ->
      let start = loc.start.pos_cnum in
      let stop = loc.stop.pos_cnum in
      let s =
        if file_contents.[start] = '#' && file_contents.[start + 1] = '|' then
          String.sub file_contents ~pos:(start + 2) ~len:(stop - start - 4)
        else
          String.sub file_contents ~pos:start ~len:(stop - start)
      in
      Comment (loc, Lines (String.split s ~on:'\n'))
  in
  loop t

let rec abstract : t -> Ast.t option = function
  | Atom (loc, atom) -> Some (Atom (loc, atom))
  | Quoted_string (loc, s) -> Some (Quoted_string (loc, s))
  | Template t -> Some (Template t)
  | List (loc, l) -> Some (List (loc, List.filter_map ~f:abstract l))
  | Comment _ -> None

let rec concrete : Ast.t -> t = function
  | Atom (loc, atom) -> Atom (loc, atom)
  | Quoted_string (loc, s) -> Quoted_string (loc, s)
  | Template t -> Template t
  | List (loc, l) -> List (loc, List.map ~f:concrete l)

let to_sexp c = abstract c |> Option.map ~f:Ast.remove_locs

let extract_comments =
  let rec loop acc = function
    | Atom _
    | Quoted_string _
    | Template _ ->
      acc
    | List (_, l) -> List.fold_left l ~init:acc ~f:loop
    | Comment (loc, comment) -> (loc, comment) :: acc
  in
  List.fold_left ~init:[] ~f:loop

let tokenize ts =
  let tokens = ref [] in
  let emit loc (token : Lexer.Token.t) = tokens := (loc, token) :: !tokens in
  let rec iter = function
    | Atom (loc, s) -> emit loc (Atom s)
    | Quoted_string (loc, s) -> emit loc (Quoted_string s)
    | Template ({ loc; _ } as template) -> emit loc (Template template)
    | Comment (loc, comment) -> emit loc (Comment comment)
    | List (loc, l) ->
      emit
        { loc with stop = { loc.start with pos_cnum = loc.start.pos_cnum + 1 } }
        Lparen;
      List.iter l ~f:iter;
      emit
        { loc with start = { loc.stop with pos_cnum = loc.stop.pos_cnum - 1 } }
        Rparen
  in
  List.iter ts ~f:iter;
  List.rev !tokens
