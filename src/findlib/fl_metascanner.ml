(* $Id: fl_metascanner.src,v 1.3 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Fl_metatoken;;


let scan ch =
  let buf = Lexing.from_channel ch in
  let rec next line pos0 =
    let t = Fl_meta.token buf in
    match t with
      Space -> next line pos0
    | Newline -> next (line + 1) (Lexing.lexeme_end buf)
    | Eof ->
        let pos = Lexing.lexeme_start buf - pos0 in
        Stream.lsing (fun _ -> line, pos, Eof)
    | _ ->
        let pos = Lexing.lexeme_start buf - pos0 in
        Stream.lcons (fun _ -> line, pos, t)
          (Stream.slazy (fun _ -> next line pos0))
  in
  next 1 0
;;


let parse ch =
  let rec mk_set l =
    match l with
      x :: l' -> if List.mem x l' then mk_set l' else x :: mk_set l'
    | [] -> []
  in
  let rec parse_all stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Name n) ->
        Stream.junk strm__;
        let props =
          try parse_properties strm__ with
            Stream.Failure ->
              raise
                (Stream.Error
                   ("Error in 'name = value' clause  in line " ^
                      string_of_int line ^ " position " ^ string_of_int col))
        in
        let rest =
          try parse_all strm__ with
            Stream.Failure ->
              raise
                (Stream.Error
                   ("Error in 'name = value' clause  in line " ^
                      string_of_int line ^ " position " ^ string_of_int col))
        in
        let (args, value) = props in
        (n, (Sort.list ( <= ) (mk_set args), value)) :: rest
    | Some (_, _, Eof) -> Stream.junk strm__; []
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error
             ("Expected 'name = value' clause  in line " ^
                string_of_int line ^ " position " ^ string_of_int col))
    | _ -> raise Stream.Failure
  and parse_properties stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, LParen) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (line1, col1, Name n) ->
            Stream.junk strm__;
            let args =
              try parse_arguments strm__ with
                Stream.Failure -> raise (Stream.Error "")
            in
            begin match Stream.peek strm__ with
              Some (line2, col2, Equal) ->
                Stream.junk strm__;
                begin match Stream.peek strm__ with
                  Some (line3, col3, String s) ->
                    Stream.junk strm__; n :: args, s
                | _ ->
                    raise
                      (Stream.Error
                         ("Expected string constant after '=' in line " ^
                            string_of_int line2 ^ " position " ^
                            string_of_int col2))
                end
            | _ ->
                raise
                  (Stream.Error
                     ("'=' expected after '(arguments)' clause in line " ^
                        string_of_int line ^ " position " ^
                        string_of_int col))
            end
        | _ ->
            raise
              (Stream.Error
                 ("After a '(' there must be an argument name in line " ^
                    string_of_int line ^ " position " ^ string_of_int col))
        end
    | Some (line, col, Equal) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (_, _, String s) -> Stream.junk strm__; [], s
        | _ ->
            raise
              (Stream.Error
                 ("'=' must be followed by a string constant in line " ^
                    string_of_int line ^ " position " ^ string_of_int col))
        end
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error
             ("Expected a '=' or a '(arguments,...)=' clause in line " ^
                string_of_int line ^ " position " ^ string_of_int col))
    | _ -> raise Stream.Failure
  and parse_arguments stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Comma) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (line1, col1, Name n) ->
            Stream.junk strm__;
            let args =
              try parse_arguments strm__ with
                Stream.Failure -> raise (Stream.Error "")
            in
            n :: args
        | _ ->
            raise
              (Stream.Error
                 ("Expected argument name after ',' in line " ^
                    string_of_int line ^ " position " ^ string_of_int col))
        end
    | Some (_, _, RParen) -> Stream.junk strm__; []
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error
             ("Another argument or a ')' expected in line " ^
                string_of_int line ^ " position " ^ string_of_int col))
    | _ -> raise Stream.Failure
  in
  let rec check l =
    match l with
      [] -> ()
    | (n, (args, value)) :: l' ->
        List.iter
          (fun (n', (args', value')) ->
             if n = n' & args = args' then
               raise
                 (Stream.Error
                    ("Double definition of '" ^ n ^ "'" ^
                       (if args = [] then ""
                        else "(" ^ String.concat "," args ^ ")"))))
          l';
        check l'
  in
  let l = parse_all (scan ch) in check l; l
;;


let lookup name predicate_list parsed_file =
  let rec search best_n best_value l =
    match l with
      [] -> if best_n >= 0 then best_value else raise Not_found
    | (name', (predicates, value)) :: l' ->
        if name = name' &
           List.for_all (fun p -> List.mem p predicate_list) predicates &
           List.length predicates > best_n then
          search (List.length predicates) value l'
        else search best_n best_value l'
  in
  search (-1) "" parsed_file
;;


(* ======================================================================
 * History:
 *
 * $Log: fl_metascanner.src,v $
 * Revision 1.3  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.2  2002/09/22 13:47:50  gerd
 * 	Changed again '?' to '??' (since O'Caml 3.04)
 *
 * Revision 1.1  2002/09/22 13:32:30  gerd
 * 	Renamed file from metascanner.src to fl_metascanner.src to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR metascanner.src:
 *
 * Revision 1.4  2001/10/12 15:02:57  gerd
 * 	Reverted from '??' syntax to '?' syntax for stream parsers.
 *
 * Revision 1.3  2001/03/06 20:12:54  gerd
 * 	Dropping O'Caml 2 support
 *
 * Revision 1.1  2000/04/26 00:09:20  gerd
 * 	O'Caml 3 changes.
 *
 *
 * Orginal log from metascanner.ml:
 *
 * Revision 1.1  1999/06/20 19:26:26  gerd
 * 	Major change: Added support for META files. In META files, knowlege
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 *
 *)
