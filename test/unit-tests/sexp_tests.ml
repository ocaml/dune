open! Stdune

let () = Printexc.record_backtrace true

(* Test that all strings of length <= 3 such that [Dsexp.Atom.is_valid
   s] are recignized as atoms by the parser *)

let string_of_syntax (x : Dsexp.syntax) =
  match x with
  | Dune -> "dune"
  | Jbuild -> "jbuild"

let () =
  [ Dsexp.Dune, Dsexp.Lexer.token, (fun s -> Dsexp.Atom.is_valid s Dune)
  ; Jbuild, Dsexp.Lexer.jbuild_token, (fun s -> Dsexp.Atom.is_valid s Jbuild)
  ]
  |> List.iter ~f:(fun (syntax, lexer, validator) ->
    for len = 0 to 3 do
      let s = Bytes.create len in
      for i = 0 to 1 lsl (len * 8) - 1 do
        if len > 0 then Bytes.set s 0 (Char.chr ( i        land 0xff));
        if len > 1 then Bytes.set s 1 (Char.chr ((i lsr 4) land 0xff));
        if len > 2 then Bytes.set s 2 (Char.chr ((i lsr 8) land 0xff));
        let s = Bytes.unsafe_to_string s in
        let parser_recognizes_as_atom =
          match Dsexp.parse_string ~lexer ~fname:"" ~mode:Single s with
          | exception _    -> false
          | Atom (_, A s') -> s = s'
          | _              -> false
        in
        let printed_as_atom =
          match Dsexp.atom_or_quoted_string s with
          | Atom _ -> true
          | _      -> false
        in
        let valid_dune_atom = validator (Dsexp.Atom.of_string s) in
        if valid_dune_atom <> parser_recognizes_as_atom then begin
          Printf.eprintf
            "Dsexp.Atom.is_valid error:\n\
             - syntax = %s\n\
             - s = %S\n\
             - Dsexp.Atom.is_valid s = %B\n\
             - parser_recognizes_as_atom = %B\n"
            (string_of_syntax syntax) s valid_dune_atom
            parser_recognizes_as_atom;
          exit 1
        end;
        if printed_as_atom && not parser_recognizes_as_atom then begin
          Printf.eprintf
            "Dsexp.Atom.atom_or_quoted_string error:\n\
            - syntax = %s\n\
             - s = %S\n\
             - printed_as_atom = %B\n\
             - parser_recognizes_as_atom = %B\n"
            (string_of_syntax syntax) s printed_as_atom
            parser_recognizes_as_atom;
          exit 1
        end
      done
    done
  )
