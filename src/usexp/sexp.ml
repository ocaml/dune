include Types.Sexp

let atom_or_quoted_string s =
  if Atom.is_valid_dune s then
    Atom (Atom.of_string s)
  else
    Quoted_string s
