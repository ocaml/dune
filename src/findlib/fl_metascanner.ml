(* $Id: fl_metascanner.src 76 2005-11-01 00:32:12Z gerd $ -*- tuareg -*-
 * ----------------------------------------------------------------------
 *
 *)

open Fl_metatoken
open Printf

type formal_pred = [ `Pred of string | `NegPred of string ]

type flavour = [ `BaseDef | `Appendix ]

type pkg_definition =
  { def_var : string;
    def_flav : flavour;
    def_preds : formal_pred list;
    def_value : string }

type pkg_expr =
  { pkg_defs : pkg_definition list; pkg_children : (string * pkg_expr) list }


let string_of_preds pl =
  let print =
    function
      `Pred n -> n
    | `NegPred n -> "-" ^ n
  in
  if pl = [] then "" else "(" ^ String.concat "," (List.map print pl) ^ ")"


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


let parse ch =
  let rec mk_set l =
    match l with
      x :: l' -> if List.mem x l' then mk_set l' else x :: mk_set l'
    | [] -> []
  in
  let error_msg m line col =
    m ^ " at line " ^ string_of_int line ^ " position " ^ string_of_int col
  in
  let rec parse_all need_rparen stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Name "package") ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (l, c, String n) ->
            Stream.junk strm__;
            begin match Stream.peek strm__ with
              Some (l, c, LParen) ->
                Stream.junk strm__;
                let subpkg =
                  try parse_all true strm__ with
                    Stream.Failure ->
                      raise
                        (Stream.Error
                           (error_msg "Error in subpackage definition" line
                              col))
                in
                let rest =
                  try parse_all need_rparen strm__ with
                    Stream.Failure -> raise (Stream.Error "")
                in
                {pkg_defs = rest.pkg_defs;
                 pkg_children = (n, subpkg) :: rest.pkg_children}
            | _ ->
                raise
                  (Stream.Error (error_msg "'(' expected after string" l c))
            end
        | _ ->
            raise
              (Stream.Error
                 (error_msg "String literal expected after 'package'" line
                    col))
        end
    | Some (line, col, Name n) ->
        Stream.junk strm__;
        let props =
          try parse_properties strm__ with
            Stream.Failure ->
              raise
                (Stream.Error
                   (error_msg "Error in 'name = value' clause" line col))
        in
        let rest =
          try parse_all need_rparen strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let (args, flav, value) = props in
        let args' = Sort.list ( <= ) (mk_set args) in
        let def =
          {def_var = n; def_flav = flav; def_preds = args'; def_value = value}
        in
        {pkg_defs = def :: rest.pkg_defs; pkg_children = rest.pkg_children}
    | Some (line, col, Eof) ->
        Stream.junk strm__;
        begin
          if need_rparen then
            raise
              (Stream.Error
                 ("Unexpected end of file in line " ^ string_of_int line ^
                    " position " ^ string_of_int col));
          {pkg_defs = []; pkg_children = []}
        end
    | Some (line, col, RParen) ->
        Stream.junk strm__;
        begin
          if not need_rparen then
            raise
              (Stream.Error
                 ("Unexpected ')' in line " ^ string_of_int line ^
                    " position " ^ string_of_int col));
          {pkg_defs = []; pkg_children = []}
        end
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error (error_msg "Expected 'name = value' clause" line col))
    | _ -> raise Stream.Failure
  and parse_properties stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, LParen) ->
        Stream.junk strm__;
        let arg1 =
          try parse_argument strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let args =
          try parse_arguments strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let flav =
          try parse_flavour strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin match Stream.peek strm__ with
          Some (line3, col3, String s) ->
            Stream.junk strm__; arg1 :: args, flav, s
        | _ ->
            raise
              (Stream.Error
                 (error_msg "Expected string constant after '='" line col))
        end
    | Some (line, col, Equal) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (_, _, String s) -> Stream.junk strm__; [], `BaseDef, s
        | _ ->
            raise
              (Stream.Error
                 (error_msg
                    "'=' must be followed by a string constant in line " line
                    col))
        end
    | Some (line, col, PlusEqual) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (_, _, String s) -> Stream.junk strm__; [], `Appendix, s
        | _ ->
            raise
              (Stream.Error
                 (error_msg
                    "'+=' must be followed by a string constant in line " line
                    col))
        end
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error
             (error_msg "Expected a '=' or a '(arguments,...)=' clause" line
                col))
    | _ -> raise Stream.Failure
  and parse_arguments stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Comma) ->
        Stream.junk strm__;
        let arg =
          try parse_argument strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let args =
          try parse_arguments strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        arg :: args
    | Some (_, _, RParen) -> Stream.junk strm__; []
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise
          (Stream.Error
             (error_msg "Another predicate or a ')' expected" line col))
    | _ -> raise Stream.Failure
  and parse_argument stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Name n) -> Stream.junk strm__; `Pred n
    | Some (line, col, Minus) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some (l, c, Name n) -> Stream.junk strm__; `NegPred n
        | _ ->
            raise
              (Stream.Error (error_msg "Name expected after '-'" line col))
        end
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise (Stream.Error (error_msg "Name or -Name expected" line col))
    | _ -> raise Stream.Failure
  and parse_flavour stream =
    let (strm__ : _ Stream.t) = stream in
    match Stream.peek strm__ with
      Some (line, col, Equal) -> Stream.junk strm__; `BaseDef
    | Some (line, col, PlusEqual) -> Stream.junk strm__; `Appendix
    | Some (line, col, _) ->
        Stream.junk strm__;
        raise (Stream.Error (error_msg "'+' or '+=' expected" line col))
    | _ -> raise Stream.Failure
  in
  let rec check_defs p l =
    match l with
      [] -> ()
    | def :: l' ->
        List.iter
          (fun def' ->
             if def.def_var = def'.def_var &&
                def.def_preds = def'.def_preds && def.def_flav = `BaseDef &&
                def'.def_flav = `BaseDef
             then
               let prefix =
                 if p = "" then "" else "In subpackage " ^ p ^ ": "
               in
               let args = string_of_preds def.def_preds in
               raise
                 (Stream.Error
                    (prefix ^ "Double definition of '" ^ def.def_var ^ args ^
                       "'")))
          l';
        check_defs p l'
  in
  let rec check_pkg p pkg =
    check_defs p pkg.pkg_defs;
    let l = ref [] in
    List.iter
      (fun (n, subpkg) ->
         let p' = if p = "" then n else p ^ "." ^ n in
         if List.mem n !l then
           raise (Stream.Error ("Double definition for subpackage " ^ p'));
         if String.contains n '.' then
           raise
             (Stream.Error
                ("Subpackage name must not contain '.': \"" ^ n ^ "\""));
         check_pkg p' subpkg;
         l := n :: !l)
      pkg.pkg_children
  in
  try let pkg = parse_all false (scan ch) in check_pkg "" pkg; pkg with
    Stream.Error "" -> raise (Stream.Error "Syntax Error")


let rec print f pkg =
  let escape s =
    let b = Buffer.create (String.length s) in
    for k = 0 to String.length s - 1 do
      match s.[k] with
        '\\' -> Buffer.add_string b "\\\\"
      | '\"' -> Buffer.add_string b "\\\""
      | c -> Buffer.add_char b c
    done;
    Buffer.contents b
  in
  let format_pred =
    function
      `Pred s -> s
    | `NegPred s -> "-" ^ s
  in
  let print_def def =
    fprintf f "%s%s %s \"%s\"\n" def.def_var
      (match def.def_preds with
         [] -> ""
       | l -> "(" ^ String.concat "," (List.map format_pred l) ^ ")")
      (match def.def_flav with
         `BaseDef -> "="
       | `Appendix -> "+=")
      (escape def.def_value)
  in
  List.iter print_def pkg.pkg_defs;
  List.iter
    (fun (name, child) ->
       fprintf f "\npackage \"%s\" (\n" (escape name);
       print f child;
       fprintf f ")\n")
    pkg.pkg_children


let lookup name predicate_list def =
  let fulfills actual_preds formal_preds =
    List.for_all
      (function
         `Pred n -> List.mem n predicate_list
       | `NegPred n -> not (List.mem n predicate_list))
      formal_preds
  in
  let rec search_base best_n best_value l =
    match l with
      [] -> if best_n >= 0 then best_value else raise Not_found
    | def :: l' ->
        if name = def.def_var && def.def_flav = `BaseDef &&
           fulfills predicate_list def.def_preds &&
           List.length def.def_preds > best_n
        then
          search_base (List.length def.def_preds) def.def_value l'
        else search_base best_n best_value l'
  in
  let rec search_appdx l =
    match l with
      [] -> []
    | def :: l' ->
        if name = def.def_var && def.def_flav = `Appendix &&
           fulfills predicate_list def.def_preds
        then
          def.def_value :: search_appdx l'
        else search_appdx l'
  in
  let step_a = search_base (-1) "" def in
  let step_b = search_appdx def in String.concat " " (step_a :: step_b)
