open OpamParserTypes
module A = Alcotest

(* utils *)

let null = ("", 0, 0)
let fd = "<nofile>"

let add_after_first ?(cond=fun _ -> true) c sep str =
  try
    let length = String.length str in
    if not (cond str) then str else
    let split = String.index str c in
    let fst = String.sub str 0 (split+1) in
    let snd = String.sub str (split+1) (length - split -1) in
    fst ^ sep ^ snd
  with _ -> failwith (Printf.sprintf "error with '%c' on %s" c str)

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

module Value = struct

  let int0 = Int (null, 0)
  let intmax = Int (null, max_int)
  let sfoo = String (null, "foo")
  let sbar = String (null, "bar")
  let sempty = String (null, "")
  let btrue = Bool (null, true)
  let bfalse = Bool (null, false)
  let a = Ident (null, "a")
  let b = Ident (null, "b")
  let c = Ident (null, "c")
  let d = Ident (null, "d")

  let simple = [
    "int", "0", int0;
    "max_int", (Printf.sprintf "%d" max_int), intmax;
    "string", "\"foo\"", sfoo;
    "string", "\"bar\"", sbar;
    "empty string", "\"\"", sempty;
    "true", "true", btrue;
    "false", "false", bfalse;
    "ident", "a", a;
    "ident", "b", b;
  ]

  let relop = [ (* printed with a space *)
    "equal",             "=",   `Eq;
    "not-equal",         "!=",  `Neq;
    "greater-or-equal",  ">=",  `Geq;
    "greater-than",      ">",   `Gt;
    "lesser-or-equal",   "<=",  `Leq;
    "lesser-than",       "<",   `Lt;
  ]
  let logop = [
    "disj",  "&",  `And;
    "conj",  "|",  `Or;
  ]
  let pfxop = [
    "not",      "!",  `Not;
    "defined",  "?",  `Defined;
  ]
  let env_update_op = [
    (*   "equal",             "=",    Eq; *)
    "plus-equal",        "+=",   PlusEq;
    "equal-plus",        "=+",   EqPlus;
    "colon-equal",       ":=",   ColonEq;
    "equal-colon",       "=:",   EqColon;
    "equal-plus-equal",  "=+=",  EqPlusEq;
  ]

  let unop ?(space=false) () =
    let (^^) a b = if space then a^" "^b else a^b in
    let args = simple in
    let fold unop f =
      List.fold_left (fun acc op -> acc @ List.map (fun arg -> f op arg) args)
        [] unop
    in
    (fold pfxop @@ fun (n,sop,op) (n', sarg, arg) ->
     (n^" "^n', sop^sarg, Pfxop (null, op, arg)))
    @
    (fold relop @@ fun (n,sop,op) (n', sarg, arg) ->
     (n^" "^n', sop^^sarg, Prefix_relop (null, op, arg)))

  let binop ?(space=false) () =
    let (^^) a b = if space then a^" "^b else a^b in
    let args =
      List.fold_left (fun acc x -> acc @ List.map (fun y -> (x,y)) simple)
        [] simple
    in
    let fold binop f =
      List.fold_left (fun acc op -> acc @ List.map (fun (arg1,arg2) ->
          f op arg1 arg2) args) [] binop
    in
    (fold relop @@ fun (n,sop,op) (n1, sarg1, arg1) (n2, sarg2, arg2) ->
     (n1^" "^n^" "^n2, sarg1^^sop^^sarg2, Relop (null, op, arg1, arg2)))
    @
    (fold logop @@ fun (n,sop,op) (n1, sarg1, arg1) (n2, sarg2, arg2) ->
     (n1^" "^n^" "^n2, sarg1^^sop^^sarg2, Logop (null, op, arg1, arg2)))
    @
    (fold env_update_op @@ fun (n,sop,op) (n1, sarg1, arg1) (n2, sarg2, arg2) ->
     (n1^" "^n^" "^n2, sarg1^^sop^^sarg2, Env_binding (null, arg1, op, arg2)))

  let lists ?(space=false) () =
    let s = if space then " " else "" in
    let slist, list = List.(split (map (fun (_,a,b) -> a,b) simple)) in
    [
      "list",
      Printf.sprintf "[%s%s%s]" s (String.concat " " slist) s,
      List (null, list);
      "group",
      Printf.sprintf "(%s%s%s)" s (String.concat " " slist) s,
      Group (null, list);
    ]
    @
    (List.map (fun (n,sarg,arg) ->
         "option "^n,
         Printf.sprintf "%s {%s%s%s}" sarg s (String.concat " " slist) s,
         Option (null, arg, list))
        simple)

  let and_or () =
    [
      "and-or", "a & (b | c)",
      Logop (null,`And, a, Group (null, [Logop (null, `Or, b, c)]));
      "or-and", "a | b & c",
      Logop (null, `Or, a, Logop (null, `And, b, c));
      "or-and", "a & b | c",
      Logop (null, `Or, Logop (null, `And, a, b), c);
      "or-and-and", "a & b | c & d",
      Logop (null, `Or, Logop (null, `And, a, b), Logop (null, `And, c, d));
      "or-and-and", "(a & b) | (c & d)",
      Logop (null, `Or, Group (null, [Logop (null, `And, a, b)]),
             Group (null, [Logop (null, `And, c, d)]));
      "or-or-and", "a | b & c | d",
      Logop (null, `Or, Logop (null, `Or, a, Logop (null, `And, b, c)), d);
      "and-or-or", "(a | b) & (c | d)",
      Logop (null, `And, Group (null, [Logop (null, `Or, a, b)]),
             Group (null, [Logop (null, `Or, c, d)]));
    ]

  let print_value v =
    OpamPrinter.value v
    |> split_on_char '\n'
    |> List.map (fun s ->
        try
          if s.[0] = ' ' then String.sub s 1 (String.length s - 1)
          else s
        with Invalid_argument _ -> s)
    |> String.concat ""

  let parse_value s =
    OpamParser.value_from_string s fd

  let value_testable =
    let value_fmt : value Fmt.t = fun ppf v ->
      Format.fprintf ppf "%s" (OpamPrinter.value v)
    in
    A.testable value_fmt OpamPrinter.value_equals

  let test_printer t () =
    List.iter (fun (name, str, value) ->
        A.check A.string name str (print_value value))
      t

  let test_parser t () =
    List.iter (fun (name, str, value) ->
        A.check value_testable name value (parse_value str))
      t

  let test_parser_printer t () =
    List.iter (fun (name, str, _) ->
        A.check A.string name str
          (print_value (parse_value str)))
      t

  let space = true
  let printer = [
    "simple",   test_printer simple;
    "unop",     test_printer @@ unop ~space ();
    "binop",    test_printer @@ binop ~space ();
    "lists",    test_printer @@ lists ();
    "and_or",   test_printer @@ and_or ();
  ]

  let parser = [
    "simple",   test_parser simple;
    "unop",     test_parser @@ unop ();
    "binop",    test_parser @@ binop ();
    "lists",    test_parser @@ lists ~space ();
    "and_or",    test_parser @@ and_or ();
  ]

  let parser_printer = [
    "simple",   test_parser_printer simple;
    "unop",     test_parser_printer @@ unop ~space ();
    "binop",    test_parser_printer @@ binop ~space ();
    "lists",    test_parser_printer @@ lists ();
    "and_or",    test_parser_printer @@ and_or ();
  ]

  let tests =
    List.fold_left (fun acc (n, t) ->
        (List.map (fun (n', t') -> n^" "^n', t') t) @ acc) []
      [
        "printer", printer;
        "parser", parser;
        "parser-printer", parser_printer;
      ]
end

module Item = struct

  let variables =
    let space = true in
    let get_name pre n =
      String.map (fun c -> if c = ' ' then '-' else c) (pre^"-"^n)
    in
    [
      List.map (fun (n,s,v) ->
          let name = get_name "simple" n in
          name, Printf.sprintf "%s: %s" name s, Variable (null, name, v))
        Value.simple;
      List.map (fun (n,s,v) ->
          let name = get_name "unop" n in
          name, Printf.sprintf "%s: %s" name s, Variable (null, name, v))
        (Value.unop ~space ());
      List.map (fun (n,s,v) ->
          let name = get_name "binop" n in
          name, Printf.sprintf "%s: %s" name s, Variable (null, name, v))
        (Value.binop ~space ());
      List.map (fun (n,s,v) ->
          let name = get_name "lists" n in
          name, Printf.sprintf "%s: %s" name s, Variable (null, name, v))
        (Value.lists  ());
    ]
    |> List.flatten

  let sections =
    let section_wname = function
      | Section (_, sec) ->
        Section (null, { sec with section_name = Some "a-name" })
      | Variable _ -> raise (Invalid_argument "section_wname")
    in
    let str_section_wname = add_after_first ' ' "\"a-name\" " in
    let sec ?(inner=false) kind items str_items =
      let padding = if inner then "  " else "" in
      kind, Printf.sprintf "%s {\n  %s%s\n%s}"
        kind padding str_items padding ,
      Section (null, { section_kind = kind;
                       section_name = None;
                       section_items = items })
    in
    let v1,s1, v2,s2, v3,s3 =
      match  variables with
      | (_,s1,v1)::_::(_,s2,v2)::_::_::(_,s3,v3)::_ ->
        v1,s1, v2,s2, v3,s3
      | _ -> assert false
    in
    let empty ?inner () = sec ?inner "section-empty" [] "" in
    let one_item ?inner () =
      sec ?inner "section-with-one-item" [v1] s1
    in
    let more_items ?inner () =
      sec ?inner "section-with-more-items" [ v1; v2; v3 ]
        (let padding = if inner = Some true then "  " else "" in
         Printf.sprintf "%s\n  %s%s\n  %s%s" s1 padding s2 padding s3)
    in
    let sec_w_esec =
      let _, estr, esec = empty ~inner:true () in
      sec "section-w-empty-section" [esec] estr
    in
    let sec_w_osec =
      let _, ostr, osec = one_item ~inner:true () in
      sec "section-w-1item-section" [osec] ostr
    in
    let sec_w_item_sec =
      let _, ostr, osec = one_item ~inner:true () in
      sec "section-w-item--asection" [v1; osec]
        (Printf.sprintf "%s\n  %s" s1 ostr)
    in
    let sec_w_sec_item =
      let _, ostr, osec = one_item ~inner:true () in
      sec "section-w-section-a-item" [osec; v2]
        (Printf.sprintf "%s\n  %s" ostr s2)
    in
    let sec_w_secs_and_items =
      let _, estr, esec = empty ~inner:true () in
      let _, ostr, osec = one_item ~inner:true () in
      let _, mstr, msec = more_items ~inner:true () in
      sec "section-w-sections-a-items"
        [ v1; esec; section_wname osec; v2; v3; msec ]
        (String.concat "\n  " [ s1; estr; str_section_wname ostr; s2; s3; mstr ])
    in
    List.fold_left (fun acc (n,s,i as section) ->
        (n^"-with-name", str_section_wname s, section_wname i)::section::acc) []
      [
        empty ();
        one_item ();
        more_items ();
        sec_w_esec;
        sec_w_osec;
        sec_w_item_sec;
        sec_w_sec_item;
        sec_w_secs_and_items;
      ] |> List.rev


  let newline =
    add_after_first ~cond:(fun s -> String.length s > 78 (* ??? *)) ':' "\n "

  let parse_item item  =
    match (OpamParser.string item fd).file_contents with
    | [x] -> x
    | _ -> failwith "irrelevant"

  let print_item item =
    OpamPrinter.items [item]

  let item_testable =
    let item_fmt : opamfile_item Fmt.t = fun ppf i ->
      Format.fprintf ppf "%s" (OpamPrinter.items [i])
    in
    A.testable item_fmt OpamPrinter.opamfile_item_equals

  let test_printer ?nl t () =
    let newline = if nl <> None then newline else fun s -> s in
    List.iter (fun (name, str, item) ->
        A.check A.string name (newline str) (print_item item))
      t

  let test_parser t () =
    List.iter (fun (name, str, item) ->
        A.check item_testable name item (parse_item str))
      t

  let test_parser_printer ?nl t () =
    let newline = if nl <> None then newline else fun s -> s in
    List.iter (fun (name, str, _) ->
        A.check A.string name (newline str)
          (print_item (parse_item str)))
      t

  let tests =
    [
      "variable printer", test_printer ~nl:true variables;
      "variable parser", test_parser variables;
      "variable parser-printer", test_parser_printer ~nl:true variables;

      "section printer", test_printer sections;
      "section parser", test_parser sections;
      "section parser-printer", test_parser_printer sections;
    ]

end

module Opamfile = struct

  let opamfiles, opamfiles_comment =
    let ofile contents = { file_contents = contents; file_name = fd } in
    let empty = "empty", "", ofile [] in
    let one_item =
      let _, str, item =
        match Item.variables with
        | x::_ -> x
        | _ -> assert false
      in
      "one-item", str, ofile [item]
    in
    let one_section =
      let _, str, section =
        match Item.sections with
        | _::x::_ -> x
        | _ -> assert false
      in
      "one-section", str, ofile [section]
    in
    let full =
      let all = Item.((List.map (fun (n,s,v) -> n, newline s, v) variables) @ sections) in
      let str = String.concat "\n" (List.map (fun (_,s,_) -> s) all) in
      let items = List.map (fun (_,_,i) -> i) all in
      "all", str, ofile items
    in
    let add_comment before (name, str, it) =
      name,
      (if before then "#tautological comment\n"^str else str^"\n#tautological comment"),
      it
    in
    [
      empty;
      one_item;
      one_section;
      full;
    ], [
      add_comment true empty;
      add_comment true one_item;
      add_comment false one_section;
    ]

  let print_ofile = OpamPrinter.opamfile
  let parse_ofile s = OpamParser.string s fd

  let ofile_testable =
    let opamfile_fmt : opamfile Fmt.t = fun ppf f ->
      Format.fprintf ppf "%s" (OpamPrinter.opamfile f)
    in
    let opamfile_equals o1 o2 =
      let rec aux o1 o2 =
        match o1, o2 with
        | i1::l1, i2::l2 ->
          OpamPrinter.opamfile_item_equals i1 i2 || aux l1 l2
        | [], [] ->  true
        | _, _ -> false
      in
      aux o1.file_contents o2.file_contents
    in
    A.testable opamfile_fmt opamfile_equals

  let test_printer t () =
    List.iter (fun (name, str, ofile) ->
        A.check A.string name str (print_ofile ofile))
      t

  let test_parser t () =
    List.iter (fun (name, str, ofile) ->
        A.check ofile_testable name ofile (parse_ofile str))
      t

  let test_parser_printer t () =
    List.iter (fun (name, str, _) ->
        A.check A.string name str
          (print_ofile (parse_ofile str)))
      t

  let positions () =
    let filename = Sys.getcwd () ^ "/sample.opam" in
    let content =
      { file_contents =
          [Variable ((filename, 1, 0), "int",
                     Int ((filename, 1, 5), 0));
           Variable ((filename, 2, 0),
                     "string",
                     String ((filename, 2, 8), "foo"));
           Section
             ((filename, 4, 0),
              {section_kind = "section"; section_name = Some "thing";
               section_items =
                 [Variable ((filename, 5, 2), "url",
                            String ((filename, 5, 7), "https://theinter.net"))
                 ]
              })
          ];
        file_name = filename}
    in
    A.check ofile_testable "sample" content (OpamParser.file filename)

  let tests =
    [
      "parser", test_parser (opamfiles @ opamfiles_comment);
      "printer", test_printer opamfiles;
      "parser-printer", test_parser_printer opamfiles;
      "positions-file", positions;
    ]

end

let _ =
  let tests = [
    "values", Value.tests;
    "items", Item.tests;
    "opamfile", Opamfile.tests;
  ] |> List.map (fun (n,t) ->
      n, List.map (fun (n,t) -> n, `Quick, t) t)
  in
  A.run "opam-file-format" tests
