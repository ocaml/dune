open Stdune

module Sample = struct
  type t =
    { name : string
    ; age : int option
    ; tags : string list
    }

  let tags =
    let open Conv in
    version (field "tags" (required (list string))) ~since:(2, 0) ~until:(3, 0)
  ;;

  let legacy =
    let open Conv in
    iso
      (record (three (field "name" (required string)) (field "age" (optional int)) tags))
      (fun (name, age, tags) -> { name; age; tags })
      (fun { name; age; tags } -> name, age, tags)
  ;;

  let fields =
    let open Conv in
    Record.make (fun name age tags -> { name; age; tags })
    |> Record.field "name" (required string) ~get:(fun { name; _ } -> name)
    |> Record.field "age" (optional int) ~get:(fun { age; _ } -> age)
    |> Record.add tags ~get:(fun { tags; _ } -> tags)
    |> Record.finish
  ;;

  let conv = Conv.record fields
end

let%expect_test "record builder preserves encoding and decoding" =
  let values =
    [ { Sample.name = ""; age = None; tags = [] }
    ; { Sample.name = "a name"; age = Some 42; tags = [ "first"; "second" ] }
    ]
  in
  List.iter values ~f:(fun value ->
    let legacy = Conv.to_sexp Sample.legacy value in
    let current = Conv.to_sexp Sample.conv value in
    print_endline (Sexp.to_string current);
    Printf.printf
      "same encoding: %b; old -> new: %b; new -> old: %b\n"
      (Sexp.equal legacy current)
      (Poly.equal (Conv.of_sexp Sample.conv ~version:(2, 0) legacy) (Ok value))
      (Poly.equal (Conv.of_sexp Sample.legacy ~version:(2, 0) current) (Ok value)));
  [%expect
    {|
    ((name "") (tags ()))
    same encoding: true; old -> new: true; new -> old: true
    ((age 42) (name "a name") (tags (first second)))
    same encoding: true; old -> new: true; new -> old: true |}]
;;

let%expect_test "record builder preserves errors, field order and versions" =
  let field name value = Sexp.List [ Atom name; value ] in
  let name = field "name" (Atom "sample") in
  let age = field "age" (Atom "42") in
  let tags = field "tags" (List []) in
  let inputs : Sexp.t list =
    [ List [ name; age; tags ]
    ; List [ tags; age; name ]
    ; List [ name; tags ]
    ; List [ name; tags; field "extra" (Atom "unexpected") ]
    ; List [ name; name; tags ]
    ; List [ tags ]
    ; List [ name ]
    ; List [ name; field "age" (Atom "invalid"); tags ]
    ; List [ name; age; field "tags" (Atom "invalid") ]
    ; List [ name; field "age" (Atom "invalid"); field "tags" (Atom "invalid") ]
    ; List [ field "name" (List []); tags ]
    ; List [ List [ Atom "name" ]; tags ]
    ; List []
    ; Atom "not a record"
    ]
  in
  List.iter
    [ 1, 9; 2, 0; 2, 1; 3, 0; 3, 1 ]
    ~f:(fun version ->
      let agrees =
        List.for_all inputs ~f:(fun sexp ->
          Poly.equal
            (Conv.of_sexp Sample.legacy ~version sexp)
            (Conv.of_sexp Sample.conv ~version sexp))
      in
      let major, minor = version in
      Printf.printf
        "%d.%d: all %d results agree: %b\n"
        major
        minor
        (List.length inputs)
        agrees);
  [%expect
    {|
    1.9: all 14 results agree: true
    2.0: all 14 results agree: true
    2.1: all 14 results agree: true
    3.0: all 14 results agree: true
    3.1: all 14 results agree: true |}]
;;

let%expect_test "record builder composes flattened field groups" =
  let fields =
    let open Conv in
    Record.make (fun sample id -> sample, id)
    |> Record.add Sample.fields ~get:fst
    |> Record.field "id" (required int) ~get:snd
    |> Record.finish
  in
  let value = { Sample.name = "sample"; age = None; tags = [] }, 7 in
  let conv = Conv.record fields in
  let sexp = Conv.to_sexp conv value in
  print_endline (Sexp.to_string sexp);
  Printf.printf
    "round trip: %b\n"
    (Poly.equal (Conv.of_sexp conv ~version:(2, 0) sexp) (Ok value));
  [%expect
    {|
    ((id 7) (name sample) (tags ()))
    round trip: true |}]
;;

let%expect_test "empty record builder" =
  let conv = Conv.(record (Record.finish (Record.make ()))) in
  Conv.to_sexp conv () |> Sexp.to_string |> print_endline;
  Printf.printf
    "empty record: %b; extra fields rejected: %b\n"
    (Poly.equal (Conv.of_sexp conv ~version:(1, 0) (List [])) (Ok ()))
    (Result.is_error
       (Conv.of_sexp conv ~version:(1, 0) (Sexp.record [ "extra", Atom "value" ])));
  [%expect
    {|
    ()
    empty record: true; extra fields rejected: true |}]
;;

module Large = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    ; d : int
    ; e : int
    ; f : int
    ; g : int
    ; h : int
    ; i : int
    }

  let conv =
    let open Conv in
    record
      (Record.make (fun a b c d e f g h i -> { a; b; c; d; e; f; g; h; i })
       |> Record.field "a" (required int) ~get:(fun { a; _ } -> a)
       |> Record.field "b" (required int) ~get:(fun { b; _ } -> b)
       |> Record.field "c" (required int) ~get:(fun { c; _ } -> c)
       |> Record.field "d" (required int) ~get:(fun { d; _ } -> d)
       |> Record.field "e" (required int) ~get:(fun { e; _ } -> e)
       |> Record.field "f" (required int) ~get:(fun { f; _ } -> f)
       |> Record.field "g" (required int) ~get:(fun { g; _ } -> g)
       |> Record.field "h" (required int) ~get:(fun { h; _ } -> h)
       |> Record.field "i" (required int) ~get:(fun { i; _ } -> i)
       |> Record.finish)
  ;;
end

let%expect_test "record builder is not limited to eight fields" =
  let value = { Large.a = 1; b = 2; c = 3; d = 4; e = 5; f = 6; g = 7; h = 8; i = 9 } in
  let sexp = Conv.to_sexp Large.conv value in
  print_endline (Sexp.to_string sexp);
  Printf.printf
    "round trip: %b\n"
    (Poly.equal (Conv.of_sexp Large.conv ~version:(1, 0) sexp) (Ok value));
  [%expect
    {|
    ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9))
    round trip: true |}]
;;

module Tree = struct
  type t =
    { label : string
    ; children : t list
    }

  let conv () =
    Conv.fixpoint (fun tree ->
      let open Conv in
      record
        (Record.make (fun label children -> { label; children })
         |> Record.field "label" (required string) ~get:(fun { label; _ } -> label)
         |> Record.field
              "children"
              (required (list tree))
              ~get:(fun { children; _ } -> children)
         |> Record.finish))
  ;;
end

let%expect_test "recursive record builder and digest" =
  let conv = Tree.conv () in
  let value = { Tree.label = "root"; children = [ { label = "leaf"; children = [] } ] } in
  let sexp = Conv.to_sexp conv value in
  print_endline (Sexp.to_string sexp);
  Printf.printf
    "round trip: %b; independently constructed digests agree: %b\n"
    (Poly.equal (Conv.of_sexp conv ~version:(1, 0) sexp) (Ok value))
    (Sexp.equal (Conv.sexp_for_digest conv) (Conv.sexp_for_digest (Tree.conv ())));
  Conv.sexp_for_digest conv |> Sexp.to_string |> print_endline;
  [%expect
    {|
    ((children (((children ()) (label leaf)))) (label root))
    round trip: true; independently constructed digests agree: true
    (Fixpoint (Record (Record_fields (Field label (Required String)) (Field children (Required (List (Recurse 0))))))) |}]
;;
