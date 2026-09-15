open Stdune
module Dune_rpc = Dune_rpc.Private
open Dune_rpc

let () = Printexc.record_backtrace false

let conv_to_digest conv =
  let sexp_string = Sexp.to_string (Conv.sexp_for_digest conv) in
  if String.length sexp_string < 32
  then sexp_string
  else Digest.to_hex (Digest.string sexp_string)
;;

let print_request_generations generations =
  List.iter generations ~f:(fun (version, Decl.Generation.T conv) ->
    Printf.printf
      "  Version %d:\n    Request: %s\n    Response: %s\n"
      version
      (conv_to_digest conv.req)
      (conv_to_digest conv.resp))
;;

let print_notification_generations generations =
  List.iter generations ~f:(fun (version, Decl.Generation.T conv) ->
    Printf.printf "  Version %d:\n    Payload: %s\n" version (conv_to_digest conv.req))
;;

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

let%expect_test "print digests for all declared RPCs" =
  List.iter Procedures.Builtin.all ~f:(fun proc ->
    match proc with
    | Procedures.Builtin.Request { decl; _ } ->
      Printf.printf "%s\n" (Method.Name.to_string decl.decl.method_);
      print_request_generations decl.generations
    | Procedures.Builtin.Notification { decl; _ } ->
      Printf.printf "%s\n" (Method.Name.to_string decl.decl.method_);
      print_notification_generations decl.generations);
  [%expect
    {|
    ping
      Version 1:
        Request: Unit
        Response: Unit
    diagnostics
      Version 1:
        Request: Unit
        Response: 310b6c3a69cc19c0eca499630848ac3e
      Version 2:
        Request: Unit
        Response: 389288a99e13b445dea30465c28c748b
    shutdown
      Version 1:
        Payload: Unit
    format
      Version 1:
        Request: Unit
        Response: Unit
    flush-file-watcher
      Version 1:
        Request: Unit
        Response: 14894520de5f9b49826c37876bedfaad
    format-dune-file
      Version 1:
        Request: 07e42be6c22c1562e5c71693ecca2cee
        Response: String
    promote
      Version 1:
        Request: String
        Response: Unit
    promote_many
      Version 1:
        Request: (Iso (List String))
        Response: 0e3f2f6008422025e109eea164914c9b
      Version 2:
        Request: d45d16a280aee5f61d5045595dfae97d
        Response: 0e3f2f6008422025e109eea164914c9b
    build_dir
      Version 1:
        Request: Unit
        Response: String
    runtest
      Version 1:
        Request: (List String)
        Response: 0e3f2f6008422025e109eea164914c9b
    dap/initialize
      Version 1:
        Request: String
        Response: 59e9b8fc460bc46600782f11ff800103
    dap/build-deps
      Version 1:
        Request: 042dc00d68ff9ee66979e82fefde258a
        Response: (Sum (None Unit) (Some String))
    notify/abort
      Version 1:
        Payload: 3c23fff9a001cfddf6d82eecd81d845f
    notify/log
      Version 1:
        Payload: 3c23fff9a001cfddf6d82eecd81d845f
    poll/running-jobs
      Version 1:
        Request: Sexp
        Response: 1faa7cea90ac2e24ad1dac47ab7360c4
    poll/diagnostic
      Version 1:
        Request: Sexp
        Response: 8cb1e41f5131e4c1f0833a9335c303f8
      Version 2:
        Request: Sexp
        Response: 1d96678d29ab01ca8adf929be6d5a077
    poll/progress
      Version 1:
        Request: Sexp
        Response: 889aa68f4ad3fc68ef5dfffbb7282c18
      Version 2:
        Request: Sexp
        Response: 929074caab98360dc7116b6f27c2b9ad
    cancel-poll/running-jobs
      Version 1:
        Payload: Sexp
    cancel-poll/diagnostic
      Version 1:
        Payload: Sexp
    cancel-poll/progress
      Version 1:
        Payload: Sexp
    |}]
;;
