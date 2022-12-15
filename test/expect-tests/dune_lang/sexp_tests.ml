open! Stdune
open Dune_lang.Decoder
open Dune_tests_common

let () = init ()

let print_loc ppf (_ : Loc.t) = Format.pp_print_string ppf "<loc>"

let sexp =
  lazy
    (Dune_lang.Parser.parse_string ~fname:"" ~mode:Single
       {|
((foo 1)
 (foo 2))
|})

let print_ast ast =
  let no_loc = Dune_lang.Ast.remove_locs ast in
  print (Dune_lang.pp no_loc)

let%expect_test _ =
  Lazy.force sexp |> print_ast;
  [%expect {|
((foo 1) (foo 2))
|}]

let of_sexp =
  let open Dune_lang.Decoder in
  enter (fields (field "foo" int))

let%expect_test _ =
  (try ignore (parse of_sexp Univ_map.empty (Lazy.force sexp) : int)
   with User_error.E msg -> User_message.print { msg with loc = None });
  [%expect {|
Error: Field "foo" is present too many times
|}]

let of_sexp : int list t = enter (fields (multi_field "foo" int))

let%expect_test _ =
  parse of_sexp Univ_map.empty (Lazy.force sexp) |> Dyn.(list int) |> print_dyn;
  [%expect {|
[ 1; 2 ]
|}]

let string_of_user_error (msg : User_message.t) =
  Format.asprintf "%a" Pp.to_fmt (User_message.pp { msg with loc = None })
  |> String.drop_prefix ~prefix:"Error: "
  |> Option.value_exn |> String.trim

let parse s =
  let res =
    try
      Ok
        (Dune_lang.Parser.parse_string ~fname:"" ~mode:Many s
        |> List.map ~f:Dune_lang.Ast.remove_locs)
    with
    | User_error.E msg -> Error (string_of_user_error msg)
    | e -> Error (Printexc.to_string e)
  in
  print_dyn (Result.to_dyn (Dyn.list Dune_lang.to_dyn) Dyn.string res)

let%expect_test _ =
  parse {| # ## x##y x||y a#b|c#d copy# |};
  [%expect {|
Ok [ "#"; "##"; "x##y"; "x||y"; "a#b|c#d"; "copy#" ]
|}]

let%expect_test _ =
  parse {|x #| comment |# y|};
  [%expect {|
Ok [ "x"; "#|"; "comment"; "|#"; "y" ]
|}]

let%expect_test _ =
  parse {|x#|y|};
  [%expect {|
Ok [ "x#|y" ]
|}]

let%expect_test _ =
  parse {|x|#y|};
  [%expect {|
Ok [ "x|#y" ]
|}]

let%expect_test _ =
  parse {|"\a"|};
  [%expect {|
Error "unknown escape sequence"
|}]

let%expect_test _ =
  parse {|"\%{x}"|};
  [%expect {|
Ok [ "%{x}" ]
|}]

let%expect_test _ =
  parse {|"$foo"|};
  [%expect {|
Ok [ "$foo" ]
|}]

let%expect_test _ =
  parse {|"%foo"|};
  [%expect {|
Ok [ "%foo" ]
|}]

let%expect_test _ =
  parse {|"bar%foo"|};
  [%expect {|
Ok [ "bar%foo" ]
|}]

let%expect_test _ =
  parse {|"bar$foo"|};
  [%expect {|
Ok [ "bar$foo" ]
|}]

let%expect_test _ =
  parse {|"%bar$foo%"|};
  [%expect {|
Ok [ "%bar$foo%" ]
|}]

let%expect_test _ =
  parse {|"$bar%foo%"|};
  [%expect {|
Ok [ "$bar%foo%" ]
|}]

let%expect_test _ =
  parse {|\${foo}|};
  [%expect {|
Ok [ "\\${foo}" ]
|}]

let%expect_test _ =
  parse {|\%{foo}|};
  [%expect {|
Ok [ template "\\%{foo}" ]
|}]

let%expect_test _ =
  parse {|\$bar%foo%|};
  [%expect {|
Ok [ "\\$bar%foo%" ]
|}]

let%expect_test _ =
  parse {|\$bar\%foo%|};
  [%expect {|
Ok [ "\\$bar\\%foo%" ]
|}]

let%expect_test _ =
  parse {|\$bar\%foo%{bar}|};
  [%expect {|
Ok [ template "\\$bar\\%foo%{bar}" ]
|}]

let%expect_test _ =
  parse {|"bar%{foo}"|};
  [%expect {|
Ok [ template "\"bar%{foo}\"" ]
|}]

let%expect_test _ =
  parse {|"bar\%{foo}"|};
  [%expect {|
Ok [ "bar%{foo}" ]
|}]

let%expect_test _ =
  parse {|bar%{foo}|};
  [%expect {|
Ok [ template "bar%{foo}" ]
|}]

let%expect_test _ =
  parse {|"bar%{foo}"|};
  [%expect {|
Ok [ template "\"bar%{foo}\"" ]
|}]

let%expect_test _ =
  parse {|"bar\%foo"|};
  [%expect {|
Ok [ "bar%foo" ]
|}]

let%expect_test _ =
  parse {|"\0000"|};
  [%expect {|
Error "unterminated decimal escape sequence"
|}]

let%expect_test _ =
  parse {|"\x000"|};
  [%expect {|
Error "unterminated hexadecimal escape sequence"
|}]

(* Printing tests *)

let loc = Loc.none

let a = Dune_lang.atom

let s x = Dune_lang.Quoted_string x

let t x = Dune_lang.Template { quoted = false; parts = x; loc }

let tq x = Dune_lang.Template { quoted = true; parts = x; loc }

let l x = Dune_lang.List x

let var ?payload name = { Dune_lang.Template.Pform.loc; name; payload }

type syntax =
  | Dune
  | Jbuild

type sexp = S of syntax * Dune_lang.t

let dyn_of_sexp (S (syntax, dlang)) =
  let open Dyn in
  variant "S"
    [ Dyn.pair
        (function
          | Dune -> Variant ("Dune", [])
          | Jbuild -> Variant ("Jbuild", []))
        Dune_lang.to_dyn (syntax, dlang)
    ]

let print_sexp ppf (S (_, sexp)) = Dune_lang.Deprecated.pp ppf sexp

type round_trip_result =
  | Round_trip_success
  | Did_not_round_trip of Dune_lang.t
  | Did_not_parse_back of string

let dyn_of_round_trip_result =
  let open Dyn in
  function
  | Round_trip_success -> variant "Round_trip_success" []
  | Did_not_round_trip s -> variant "Did_not_round_trip" [ Dune_lang.to_dyn s ]
  | Did_not_parse_back s -> variant "Did_not_parse_back" [ string s ]

let test syntax sexp =
  let res =
    ( S (syntax, sexp)
    , let s =
        Format.asprintf "%a" (fun ppf x -> Pp.to_fmt ppf (Dune_lang.pp x)) sexp
      in
      match Dune_lang.Parser.parse_string s ~mode:Single ~fname:"" with
      | sexp' ->
        let sexp' = Dune_lang.Ast.remove_locs sexp' in
        if sexp = sexp' then Round_trip_success else Did_not_round_trip sexp'
      | exception User_error.E msg ->
        Did_not_parse_back (string_of_user_error msg) )
  in
  let open Dyn in
  pair dyn_of_sexp dyn_of_round_trip_result res |> print_dyn

let%expect_test _ =
  test Dune (a "toto");
  [%expect {|
(S (Dune, "toto"), Round_trip_success)
|}]

let%expect_test _ =
  test Dune (t [ Text "x%{" ]);
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "({ pos_fname = \"<none>\"\
   \n ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }\
   \n ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }\
   \n },\
   \n\"Invalid text in unquoted template\", { s = \"x%{\" })") |}]

let%expect_test _ =
  test Dune (t [ Text "x%"; Text "{" ]);
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "({ pos_fname = \"<none>\"\
   \n ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }\
   \n ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }\
   \n },\
   \n\"Invalid text in unquoted template\", { s = \"x%{\" })") |}]

let%expect_test _ =
  (* This round trip failure is expected *)
  test Dune (tq [ Text "x%{" ]);
  [%expect {|
(S (Dune, template "\"x\\%{\""), Did_not_round_trip "x%{")
|}]

let%expect_test _ =
  test Dune (tq [ Text "x%"; Text "{" ]);
  [%expect {|
(S (Dune, template "\"x\\%{\""), Did_not_round_trip "x%{")
|}]

let%expect_test _ =
  (* Check parsing of comments *)
  Dune_lang.Parser.parse ~mode:Cst
    (Lexing.from_string
       {|
hello
; comment
world

; multiline
; comment

(x ; comment inside list
 y)
|})
  |> Dyn.list Dune_lang.Cst.to_dyn
  |> print_dyn;
  [%expect
    {|
[ Atom A "hello"
; Comment [ " comment" ]
; Atom A "world"
; Comment [ " multiline"; " comment" ]
; List [ Atom A "x"; Comment [ " comment inside list" ]; Atom A "y" ]
]
|}]

let jbuild_file =
  {|
hello
; comment
world

; multiline
; comment

(x ; comment inside list
 y)

#; (sexp
comment)

#|old style
block
comment|#
|}
