open! Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Dune_lang.Decoder.t
end

let latest_version = (1, 12)

let syntax =
  Syntax.create ~name:"dune" ~desc:"the dune language"
    [ (0, 0) (* Jbuild syntax *)
    ; latest_version
    ]

module File_kind = struct
  type t = Dune_lang.File_syntax.t = Jbuild | Dune

  let of_syntax = function
    | (0, _) -> Jbuild
    | (_, _) -> Dune
end

let file_kind () =
  let open Dune_lang.Decoder in
  Syntax.get_exn syntax >>| File_kind.of_syntax

module Decoder = struct
  include Dune_lang.Decoder

  exception Parens_no_longer_necessary of Loc.t * exn

  let switch_file_kind ~jbuild ~dune =
    file_kind () >>= function
    | Jbuild -> jbuild
    | Dune -> dune

  let parens_removed_in_dune_generic ~is_record t =
    switch_file_kind
      ~jbuild:(enter t)
      ~dune:(
        try_
          t
          (function
            | Parens_no_longer_necessary _ as exn -> raise exn
            | exn ->
              try_
                (enter
                   (let* loc = loc in
                    let* _ =
                      if is_record then
                        peek >>= function
                        | Some (List _) ->
                          raise (Parens_no_longer_necessary (loc, exn))
                        | _ -> t
                      else
                        t
                    in
                    raise (Parens_no_longer_necessary (loc, exn))))
                (function
                  | Parens_no_longer_necessary _ as exn -> raise exn
                  | _ -> raise exn))
      )

  let record parse =
    parens_removed_in_dune_generic (fields parse) ~is_record:true

  let parens_removed_in_dune t =
    parens_removed_in_dune_generic t ~is_record:false

  let list parse =
    parens_removed_in_dune (repeat parse)

  let on_dup parsing_context name entries =
    match Univ_map.find parsing_context (Syntax.key syntax) with
    | Some (0, _) ->
      (* DUNE2: delete this branch (0.x is for jbuilder compat) *)
      let last = Option.value_exn (List.last entries) in
      User_warning.emit ~loc:(Dune_lang.Ast.loc last)
        [ Pp.textf "Field %S is present several times, previous \
                    occurrences are ignored."
            name
        ]
    | _ ->
      field_present_too_many_times parsing_context name entries

  let field name ?default t = field name ?default t ~on_dup
  let field_o name t = field_o name t ~on_dup
  let field_b ?check name = field_b name ?check ~on_dup
  let field_o_b ?check name = field_o_b name ?check ~on_dup
end
