open Stdune

type sample =
  { label : string
  ; score : int option
  ; marks : char array
  ; tags : string list
  }

let sample_repr =
  Repr.record
    "sample"
    [ Repr.field "label" Repr.string ~get:(fun { label; _ } -> label)
    ; Repr.field "score" Repr.(option int) ~get:(fun { score; _ } -> score)
    ; Repr.field "marks" Repr.(array char) ~get:(fun { marks; _ } -> marks)
    ; Repr.field "tags" Repr.(list string) ~get:(fun { tags; _ } -> tags)
    ]
;;

type tree =
  | Leaf of int
  | Node of tree list

let tree_repr =
  Repr.fix (fun repr ->
    Repr.variant
      "tree"
      [ Repr.case "Leaf" Repr.int ~proj:(function
          | Leaf value -> Some value
          | Node _ -> None)
      ; Repr.case "Node" (Repr.list repr) ~proj:(function
          | Leaf _ -> None
          | Node children -> Some children)
      ])
;;

let print_code_error thunk =
  match thunk () with
  | () -> print_endline "ok"
  | exception Code_error.E e ->
    print_endline (Dyn.to_string (Code_error.to_dyn_without_loc e))
;;

let%expect_test "make_compare accepts structural reprs" =
  let equal_sample, compare_sample = Repr.make_compare sample_repr in
  let left =
    { label = "a"; score = Some 1; marks = [| 'x'; 'y' |]; tags = [ "m"; "n" ] }
  in
  let right =
    { label = "a"; score = Some 1; marks = [| 'x'; 'y' |]; tags = [ "m"; "n" ] }
  in
  let later = { label = "b"; score = None; marks = [| 'z' |]; tags = [ "m" ] } in
  print_endline (Bool.to_string (equal_sample left right));
  print_endline (Ordering.to_string (compare_sample left later));
  let equal_tree, compare_tree = Repr.make_compare tree_repr in
  let tree = Node [ Leaf 1; Node [ Leaf 2 ] ] in
  print_endline (Bool.to_string (equal_tree tree tree));
  print_endline (Ordering.to_string (compare_tree (Leaf 1) (Node [ Leaf 1 ])));
  let equal_t4, compare_t4 =
    Repr.make_compare (Repr.T4.repr Repr.int Repr.string Repr.bool Repr.char)
  in
  print_endline (Bool.to_string (equal_t4 (1, "x", true, 'z') (1, "x", true, 'z')));
  print_endline (Ordering.to_string (compare_t4 (1, "x", true, 'a') (2, "x", true, 'a')));
  [%expect
    {|
    true
    <
    true
    <
    true
    <
  |}]
;;

let%expect_test "make_compare rejects floats view and abstract reprs" =
  print_code_error (fun () -> ignore (Repr.make_compare Repr.float));
  print_code_error (fun () -> ignore (Repr.make_compare Repr.(option float)));
  print_code_error (fun () ->
    ignore (Repr.make_compare (Repr.view Repr.string ~to_:string_of_int)));
  print_code_error (fun () -> ignore (Repr.make_compare (Repr.abstract Dyn.int)));
  [%expect
    {|
    ("Repr.make_compare: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "float" })
    ("Repr.make_compare: repr is not sound for polymorphic comparison",
     { path = [ "root"; "option" ]; repr_kind = "float" })
    ("Repr.make_compare: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "view" })
    ("Repr.make_compare: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "abstract" })
  |}]
;;
