open Stdune
module Dune_rpc = Dune_rpc.Private
open Dune_rpc

let () = Printexc.record_backtrace false

let%expect_test "sexp_for_digest" =
  let print_sexp_for_digest conv =
    Pp.to_fmt Format.std_formatter (Sexp.pp (Conv.sexp_for_digest conv))
  in
  print_sexp_for_digest
    (Conv.five
       (Conv.field "a" (Conv.required Conv.string))
       (Conv.field "b" (Conv.optional Conv.int))
       (Conv.field "c" (Conv.required Conv.float))
       (Conv.field "d" (Conv.required Conv.unit))
       (Conv.field "e" (Conv.optional Conv.char)));
  [%expect
    {|
      (Iso
       (Both
        (Both (Field a (Required String)) (Field b (Optional Int)))
        (Iso
         (Both
          (Field c (Required Float))
          (Both (Field d (Required Unit)) (Field e (Optional Char))))))) |}];
  print_sexp_for_digest (Conv.iso Conv.sexp (fun x -> x) (fun x -> x));
  [%expect {| (Iso Sexp) |}];
  let id_iso = Conv.iso Conv.sexp (fun x -> x) (fun x -> x) in
  print_sexp_for_digest
    (Conv.pair
       (Conv.version ~until:(1, 2) ~since:(2, 3) id_iso)
       (Conv.version ~since:(1, 2) id_iso));
  [%expect
    {|
    (Pair
     (Version (Iso Sexp) (since 2 3) (until 1 2))
     (Version (Iso Sexp) (since 1 2))) |}];
  let list_conv inner =
    Conv.fixpoint (fun conv ->
      let nil = Conv.constr "nil" Conv.unit (fun () -> []) in
      let cons = Conv.constr "cons" (Conv.pair inner conv) (fun (x, xs) -> x :: xs) in
      Conv.sum
        [ Conv.econstr nil; Conv.econstr cons ]
        (function
          | [] -> Conv.case () nil
          | x :: xs -> Conv.case (x, xs) cons))
  in
  print_sexp_for_digest (list_conv Conv.int);
  [%expect {| (Fixpoint (Sum (nil Unit) (cons (Pair Int (Recurse 0))))) |}];
  print_sexp_for_digest (list_conv (list_conv Conv.int));
  (* Recursion uses De Bruijn indices because we want equal structures to
     produce the same digest. *)
  [%expect
    {|
    (Fixpoint
     (Sum
      (nil Unit)
      (cons
       (Pair
        (Fixpoint (Sum (nil Unit) (cons (Pair Int (Recurse 0)))))
        (Recurse 0))))) |}]
;;

let%expect_test "print digests for all public RPCs" =
  Decl.Request.print_generations Procedures.Public.ping;
  [%expect
    {|
    Version 1:
      Request: Unit
      Response: Unit
    |}];
  Decl.Request.print_generations Procedures.Public.diagnostics;
  [%expect
    {|
    Version 1:
      Request: Unit
      Response: ffd3de9652c685594aacfc51d28f2533
    Version 2:
      Request: Unit
      Response: 0d4442e0c36d6727a9acf9aabce6a6ad
    |}];
  Decl.Notification.print_generations Procedures.Public.shutdown;
  [%expect {| Version 1: Unit |}];
  Decl.Request.print_generations Procedures.Public.format_dune_file;
  [%expect
    {|
    Version 1:
      Request: 15eae4b546faf05a0fc3b6d03aed0c63
      Response: String
    |}];
  Decl.Request.print_generations Procedures.Public.promote;
  [%expect
    {|
    Version 1:
      Request: String
      Response: Unit
    |}];
  Decl.Request.print_generations Procedures.Public.build_dir;
  [%expect
    {|
    Version 1:
      Request: Unit
      Response: String
    |}];
  Decl.Notification.print_generations Procedures.Server_side.abort;
  [%expect {| Version 1: 0e9dfd1099101769896cf0bb06f891c6 |}];
  Decl.Notification.print_generations Procedures.Server_side.log;
  [%expect {| Version 1: 0e9dfd1099101769896cf0bb06f891c6 |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.progress);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 889aa68f4ad3fc68ef5dfffbb7282c18
    Version 2:
      Request: Sexp
      Response: 929074caab98360dc7116b6f27c2b9ad
    |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.diagnostic);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 443627a52ab5595206164d020ff01c56
    Version 2:
      Request: Sexp
      Response: 12995aa06697c01ef35c0339bd2fa29e
    |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.running_jobs);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 33528f248084297d123a6ebd4c3ddee0
    |}]
;;
