open Stdune

let%expect_test "predicate manifests preserve their exact representation" =
  let open Predicate_lang in
  let literal = Glob.of_string_list [ "a[*]"; "a\nb" ] in
  let predicates =
    [ true_
    ; false_
    ; standard
    ; literal
    ; Glob.of_glob (Dune_lang.Glob.of_string "a*")
    ; not literal
    ; and_ [ literal; standard ]
    ; or_ [ literal; standard ]
    ; of_list []
    ; not (or_ [ and_ [ literal; standard ]; not literal ])
    ]
  in
  let roundtrips =
    List.for_all predicates ~f:(fun original ->
      match Conv.of_sexp Glob.conv ~version:(0, 0) (Conv.to_sexp Glob.conv original) with
      | Error _ -> false
      | Ok restored -> Glob.equal original restored)
  in
  Printf.printf "roundtrips: %b\n" roundtrips;
  let invalid =
    let open Sexp in
    [ Atom "invalid"
    ; List [ Atom "true"; Atom "extra" ]
    ; List [ Atom "not"; Atom "invalid" ]
    ; List [ Atom "or"; List [ Atom "invalid" ] ]
    ; List [ Atom "literal"; List [] ]
    ]
  in
  Printf.printf
    "invalid: %b\n"
    (List.for_all invalid ~f:(fun sexp ->
       Result.is_error (Conv.of_sexp Glob.conv ~version:(0, 0) sexp)));
  [%expect
    {|
    roundtrips: true
    invalid: true
    |}]
;;
