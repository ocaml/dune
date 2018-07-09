open Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Sexp.Of_sexp.t
end

let syntax =
  Syntax.create ~name:"dune" ~desc:"the dune language"
    [ (0, 0) (* Jbuild syntax *)
    ; (1, 0)
    ]

module File_kind = struct
  type t = Sexp.syntax = Jbuild | Dune

  let of_syntax = function
    | (0, _) -> Jbuild
    | (_, _) -> Dune
end

let file_kind () =
  let open Sexp.Of_sexp in
  Syntax.get_exn syntax >>| fun ver ->
  if ver < (1, 0) then File_kind.Jbuild else Dune

module Of_sexp = struct
  include Sexp.Of_sexp

  let record parse =
    Syntax.get_exn syntax >>= fun ver ->
    if ver < (1, 0) then
      record parse
    else
      fields parse

  let list parse =
    Syntax.get_exn syntax >>= fun ver ->
    if ver < (1, 0) then
      list parse
    else
      repeat parse

  let on_dup parsing_context name entries =
    match Univ_map.find parsing_context (Syntax.key syntax) with
    | Some (0, _) ->
      let last = Option.value_exn (List.last entries) in
      Loc.warn (Sexp.Ast.loc last)
        "Field %S is present several times, previous occurrences are ignored."
        name
    | _ ->
      field_present_too_many_times parsing_context name entries

  let field name ?default t = field name ?default t ~on_dup
  let field_o name t = field_o name t ~on_dup
  let field_b ?check name = field_b name ?check ~on_dup
end
