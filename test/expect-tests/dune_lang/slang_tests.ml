open Dune_lang

let () = Dune_tests_common.init ()
let const b : Slang.blang = Blang.Const b
let not_ b : Slang.blang = Blang.Not b
let and_ bs : Slang.blang = Blang.And bs
let or_ bs : Slang.blang = Blang.Or bs
let expr t : Slang.blang = Blang.Expr t
let compare op a b : Slang.blang = Blang.Compare (op, a, b)

let pform name =
  Slang.pform
    (Pform.Macro { macro = Pform.Macro.Pkg_self; payload = Pform.Payload.of_string name })
;;

let print_slang (s : Slang.t) =
  Slang.simplify s
  |> Slang.remove_locs
  |> Slang.to_dyn
  |> Dyn.pp
  |> Stdlib.Format.printf "%a@." Pp.to_fmt
;;

let print_blang (b : Slang.blang) =
  Slang.simplify_blang b
  |> Slang.Blang.remove_locs
  |> Slang.Blang.to_dyn
  |> Dyn.pp
  |> Stdlib.Format.printf "%a@." Pp.to_fmt
;;

(* Blang: Const *)

let%expect_test "const true" =
  print_blang (const true);
  [%expect {| Const true |}]
;;

let%expect_test "const false" =
  print_blang (const false);
  [%expect {| Const false |}]
;;

(* Blang: Expr *)

let%expect_test "expr true literal" =
  print_blang (expr (Slang.text "true"));
  [%expect {| Const true |}]
;;

let%expect_test "expr false literal" =
  print_blang (expr (Slang.text "false"));
  [%expect {| Const false |}]
;;

let%expect_test "expr blang true" =
  print_blang (expr (Slang.bool true));
  [%expect {| Const true |}]
;;

let%expect_test "expr blang false" =
  print_blang (expr (Slang.bool false));
  [%expect {| Const false |}]
;;

let%expect_test "expr pform" =
  print_blang (expr (pform "x"));
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

(* CR-soon Alizter: should reduce to Const false *)
let%expect_test "expr (blang const)" =
  let inner = Slang.catch_undefined_var (Slang.bool false) ~fallback:(Slang.bool true) in
  print_blang (expr inner);
  [%expect {| Expr (Catch_undefined_var (Blang (Const false), Blang (Const true))) |}]
;;

(* Blang: And *)

let%expect_test "and singleton" =
  print_blang (and_ [ expr (pform "x") ]);
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

let%expect_test "and false short-circuits" =
  print_blang (and_ [ expr (pform "x"); const false; expr (pform "y") ]);
  [%expect {| Const false |}]
;;

let%expect_test "and filters true" =
  print_blang (and_ [ const true; expr (pform "x") ]);
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

let%expect_test "and all true" =
  print_blang (and_ [ const true; const true ]);
  [%expect {| Const true |}]
;;

let%expect_test "and flattens" =
  print_blang (and_ [ and_ [ expr (pform "a"); expr (pform "b") ]; expr (pform "c") ]);
  [%expect
    {|
    And
      (Expr (Literal (template "%{pkg-self:a}")),
       Expr (Literal (template "%{pkg-self:b}")),
       Expr (Literal (template "%{pkg-self:c}")))
    |}]
;;

(* Blang: Or *)

let%expect_test "or singleton" =
  print_blang (or_ [ expr (pform "x") ]);
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

let%expect_test "or true short-circuits" =
  print_blang (or_ [ expr (pform "x"); const true; expr (pform "y") ]);
  [%expect {| Const true |}]
;;

let%expect_test "or filters false" =
  print_blang (or_ [ const false; expr (pform "x") ]);
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

let%expect_test "or all false" =
  print_blang (or_ [ const false; const false ]);
  [%expect {| Const false |}]
;;

let%expect_test "or flattens" =
  print_blang (or_ [ or_ [ expr (pform "a"); expr (pform "b") ]; expr (pform "c") ]);
  [%expect
    {|
    Or
      (Expr (Literal (template "%{pkg-self:a}")),
       Expr (Literal (template "%{pkg-self:b}")),
       Expr (Literal (template "%{pkg-self:c}")))
    |}]
;;

(* Blang: Not *)

let%expect_test "not unknown" =
  print_blang (not_ (expr (pform "x")));
  [%expect {| Not (Expr (Literal (template "%{pkg-self:x}"))) |}]
;;

let%expect_test "not true" =
  print_blang (not_ (const true));
  [%expect {| Const false |}]
;;

let%expect_test "not false" =
  print_blang (not_ (const false));
  [%expect {| Const true |}]
;;

let%expect_test "not not" =
  print_blang (not_ (not_ (expr (pform "x"))));
  [%expect {| Expr (Literal (template "%{pkg-self:x}")) |}]
;;

(* Blang: Compare *)

let%expect_test "compare unknown" =
  print_blang (compare Relop.Eq (pform "x") (pform "y"));
  [%expect
    {|
    Compare
      (=, Literal (template "%{pkg-self:x}"), Literal (template "%{pkg-self:y}"))
    |}]
;;

let%expect_test "compare mixed" =
  print_blang (compare Relop.Eq (Slang.text "a") (pform "x"));
  [%expect {| Compare (=, Literal "a", Literal (template "%{pkg-self:x}")) |}]
;;

(* CR-soon Alizter: should reduce to Const true *)
let%expect_test "compare eq same" =
  print_blang (compare Relop.Eq (Slang.text "a") (Slang.text "a"));
  [%expect {| Compare (=, Literal "a", Literal "a") |}]
;;

(* CR-soon Alizter: should reduce to Const false *)
let%expect_test "compare eq different" =
  print_blang (compare Relop.Eq (Slang.text "a") (Slang.text "b"));
  [%expect {| Compare (=, Literal "a", Literal "b") |}]
;;

(* CR-soon Alizter: should reduce to Const false *)
let%expect_test "compare neq same" =
  print_blang (compare Relop.Neq (Slang.text "a") (Slang.text "a"));
  [%expect {| Compare (<>, Literal "a", Literal "a") |}]
;;

(* CR-soon Alizter: should reduce to Const true *)
let%expect_test "compare neq different" =
  print_blang (compare Relop.Neq (Slang.text "a") (Slang.text "b"));
  [%expect {| Compare (<>, Literal "a", Literal "b") |}]
;;

(* Ordering comparisons are not reduced because comparison semantics depend on
   context (e.g. version comparison vs string comparison). *)

let%expect_test "compare lt" =
  print_blang (compare Relop.Lt (Slang.text "a") (Slang.text "b"));
  [%expect {| Compare (<, Literal "a", Literal "b") |}]
;;

let%expect_test "compare le" =
  print_blang (compare Relop.Lte (Slang.text "a") (Slang.text "a"));
  [%expect {| Compare (<=, Literal "a", Literal "a") |}]
;;

let%expect_test "compare gt" =
  print_blang (compare Relop.Gt (Slang.text "b") (Slang.text "a"));
  [%expect {| Compare (>, Literal "b", Literal "a") |}]
;;

let%expect_test "compare ge" =
  print_blang (compare Relop.Gte (Slang.text "a") (Slang.text "a"));
  [%expect {| Compare (>=, Literal "a", Literal "a") |}]
;;

(* Slang: Nil/Literal *)

let%expect_test "nil" =
  print_slang Slang.Nil;
  [%expect {| Nil |}]
;;

let%expect_test "literal" =
  print_slang (Slang.text "hello");
  [%expect {| Literal "hello" |}]
;;

(* Slang: Concat

   Concat simplification combines all Literal elements (text and pforms) into a
   single String_with_vars template. Non-Literal elements (When, If, etc.)
   prevent full simplification. *)

let%expect_test "concat empty" =
  print_slang (Slang.concat []);
  [%expect {| Literal "" |}]
;;

let%expect_test "concat singleton" =
  print_slang (Slang.concat [ Slang.text "hello" ]);
  [%expect {| Literal "hello" |}]
;;

let%expect_test "concat filters nil" =
  print_slang (Slang.concat [ Slang.text "a"; Slang.Nil; Slang.text "b" ]);
  [%expect {| Literal (template "ab") |}]
;;

let%expect_test "concat combines literals" =
  print_slang (Slang.concat [ Slang.text "a"; Slang.text "b"; Slang.text "c" ]);
  [%expect {| Literal (template "abc") |}]
;;

let%expect_test "concat combines pforms" =
  print_slang (Slang.concat [ pform "x"; pform "y" ]);
  [%expect {| Literal (template "%{pkg-self:x}%{pkg-self:y}") |}]
;;

let%expect_test "concat combines mixed" =
  print_slang (Slang.concat [ Slang.text "a"; pform "x"; Slang.text "b" ]);
  [%expect {| Literal (template "a%{pkg-self:x}b") |}]
;;

let%expect_test "concat non-literal" =
  print_slang
    (Slang.concat [ Slang.text "a"; Slang.when_ (expr (pform "c")) (Slang.text "b") ]);
  [%expect
    {|
    Concat
      (Literal "a", When (Expr (Literal (template "%{pkg-self:c}")), Literal "b"))
    |}]
;;

(* Slang: When *)

let%expect_test "when true" =
  print_slang (Slang.when_ (const true) (Slang.text "hello"));
  [%expect {| Literal "hello" |}]
;;

let%expect_test "when false" =
  print_slang (Slang.when_ (const false) (Slang.text "hello"));
  [%expect {| Nil |}]
;;

let%expect_test "when unknown" =
  print_slang (Slang.when_ (expr (pform "x")) (Slang.text "hello"));
  [%expect {| When (Expr (Literal (template "%{pkg-self:x}")), Literal "hello") |}]
;;

(* Slang: If *)

let%expect_test "if true" =
  print_slang (Slang.if_ (const true) ~then_:(Slang.text "yes") ~else_:(Slang.text "no"));
  [%expect {| Literal "yes" |}]
;;

let%expect_test "if false" =
  print_slang (Slang.if_ (const false) ~then_:(Slang.text "yes") ~else_:(Slang.text "no"));
  [%expect {| Literal "no" |}]
;;

let%expect_test "if unknown" =
  print_slang
    (Slang.if_ (expr (pform "x")) ~then_:(Slang.text "yes") ~else_:(Slang.text "no"));
  [%expect
    {| If (Expr (Literal (template "%{pkg-self:x}")), Literal "yes", Literal "no") |}]
;;

(* CR-soon Alizter: should reduce to Blang (Expr ...) *)
let%expect_test "if eta: true/false" =
  print_slang
    (Slang.if_ (expr (pform "c")) ~then_:(Slang.bool true) ~else_:(Slang.bool false));
  [%expect
    {|
    If
      (Expr (Literal (template "%{pkg-self:c}")),
       Blang (Const true),
       Blang (Const false))
    |}]
;;

(* CR-soon Alizter: should reduce to Blang (Not ...) *)
let%expect_test "if eta: false/true" =
  print_slang
    (Slang.if_ (expr (pform "c")) ~then_:(Slang.bool false) ~else_:(Slang.bool true));
  [%expect
    {|
    If
      (Expr (Literal (template "%{pkg-self:c}")),
       Blang (Const false),
       Blang (Const true))
    |}]
;;

(* CR-soon Alizter: should reduce to Literal "same" *)
let%expect_test "if eta: same branches" =
  print_slang
    (Slang.if_ (expr (pform "c")) ~then_:(Slang.text "same") ~else_:(Slang.text "same"));
  [%expect
    {|
    If
      (Expr (Literal (template "%{pkg-self:c}")), Literal "same", Literal "same")
    |}]
;;

(* Slang: Catch_undefined_var *)

(* CR-soon Alizter: should reduce to Blang (Const false) *)
let%expect_test "catch const" =
  print_slang (Slang.catch_undefined_var (Slang.bool false) ~fallback:(Slang.bool true));
  [%expect {| Catch_undefined_var (Blang (Const false), Blang (Const true)) |}]
;;

let%expect_test "catch unknown" =
  print_slang (Slang.catch_undefined_var (pform "x") ~fallback:(Slang.bool false));
  [%expect
    {| Catch_undefined_var (Literal (template "%{pkg-self:x}"), Blang (Const false)) |}]
;;

(* Slang: Has_undefined_var *)

let%expect_test "has_undefined_var pform" =
  print_slang (Slang.has_undefined_var (pform "x"));
  [%expect {| Has_undefined_var (Literal (template "%{pkg-self:x}")) |}]
;;

(* CR-soon Alizter: should reduce to Blang (Const false) *)
let%expect_test "has_undefined_var const" =
  print_slang (Slang.has_undefined_var (Slang.bool true));
  [%expect {| Has_undefined_var (Blang (Const true)) |}]
;;

(* Slang: And_absorb_undefined_var *)

let%expect_test "and_absorb flattens" =
  print_slang
    (Slang.and_absorb_undefined_var
       [ expr (Slang.and_absorb_undefined_var [ expr (pform "a"); expr (pform "b") ])
       ; expr (pform "c")
       ]);
  [%expect
    {|
    And_absorb_undefined_var
      (Expr (Literal (template "%{pkg-self:a}")),
       Expr (Literal (template "%{pkg-self:b}")),
       Expr (Literal (template "%{pkg-self:c}")))
    |}]
;;

(* Slang: Or_absorb_undefined_var *)

let%expect_test "or_absorb flattens" =
  print_slang
    (Slang.or_absorb_undefined_var
       [ expr (Slang.or_absorb_undefined_var [ expr (pform "a"); expr (pform "b") ])
       ; expr (pform "c")
       ]);
  [%expect
    {|
    Or_absorb_undefined_var
      (Expr (Literal (template "%{pkg-self:a}")),
       Expr (Literal (template "%{pkg-self:b}")),
       Expr (Literal (template "%{pkg-self:c}")))
    |}]
;;
