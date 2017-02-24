open Import

let single fn f =
  let sexp, locs =
    with_lexbuf_from_file fn ~f:Sexp_lexer.single
  in
  try
    f sexp
  with Sexp.Of_sexp_error (msg, sub) ->
    let loc =
      match Sexp.locate sexp ~sub ~locs with
      | None -> Loc.in_file fn
      | Some loc -> loc
    in
    Loc.fail loc "%s" msg

let many fn f =
  let sexps, locs =
    with_lexbuf_from_file fn ~f:Sexp_lexer.many
    |> List.split
  in
  try
    f sexps
  with Sexp.Of_sexp_error (msg, sub) ->
    let loc =
      match Sexp.locate_in_list sexps ~sub ~locs with
      | None -> Loc.in_file fn
      | Some loc -> loc
    in
    Loc.fail loc "%s" msg
