include Sexp0

module List = ListLabels

let rec pp ppf = function
  | Atom s ->
    Format.pp_print_string ppf (Escape.quote_if_needed s)
  | List [] ->
    Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp ppf first;
    List.iter rest ~f:(fun sexp ->
      Format.pp_print_space ppf ();
      pp ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()
