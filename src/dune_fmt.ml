open! Stdune
open! Import

let parse_file path_opt =
  let fname, contents =
    match path_opt with
    | Some path ->
      Io.with_file_in path ~f:(fun ic ->
        let contents = Io.read_all ic in
        (Path.to_string path, contents)
      )
    | None ->
      let lines = Io.input_lines stdin in
      let contents = String.concat ~sep:"\n" lines in
      ("<stdin>", contents)
  in
  Dune_lang.parse_string
    ~fname
    ~mode:Dune_lang.Parser.Mode.Many
    contents

let can_be_displayed_wrapped =
  List.for_all ~f:(function
    | Dune_lang.Atom _
    | Dune_lang.Quoted_string _
    | Dune_lang.Template _
    | Dune_lang.List [_]
      ->
      true
    | Dune_lang.List _
      ->
      false
  )

let print_wrapped_list fmt =
  Format.fprintf fmt "(@[<hov 1>%a@])"
    (Fmt.list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
      (Dune_lang.pp Dune_lang.Dune)
    )

let rec pp_sexp fmt =
  function
    ( Dune_lang.Atom _
    | Dune_lang.Quoted_string _
    | Dune_lang.Template _
    ) as sexp
    ->
    Format.fprintf fmt "%a"
      (Dune_lang.pp Dune_lang.Dune) sexp
  | Dune_lang.List sexps
    ->
    Format.fprintf fmt "@[<v 1>%a@]"
      (if can_be_displayed_wrapped sexps then
         print_wrapped_list
       else
         pp_sexp_list)
      sexps

and pp_sexp_list fmt =
  let pp_sep fmt () = Format.fprintf fmt "@," in
  Format.fprintf fmt "(%a)"
    (Fmt.list ~pp_sep pp_sexp)

let pp_top_sexp fmt sexp =
  Format.fprintf fmt "%a\n" pp_sexp sexp

let pp_top_sexps =
  Fmt.list
    ~pp_sep:Fmt.nl
    (fun fmt sexp ->
      pp_top_sexp fmt (Dune_lang.Ast.remove_locs sexp))

let with_output path_opt k =
  match path_opt with
  | None ->
    k Format.std_formatter
  | Some path ->
    Io.with_file_out ~binary:true path ~f:(fun oc ->
      k @@ Format.formatter_of_out_channel oc
    )

let format_file ~input ~output =
  match parse_file input with
  | exception Dune_lang.Parse_error e ->
    Printf.printf
      "Parse error: %s\n"
      (Dune_lang.Parse_error.message e)
  | sexps ->
    with_output output (fun fmt ->
      pp_top_sexps fmt sexps;
      Format.pp_print_flush fmt ()
    )
