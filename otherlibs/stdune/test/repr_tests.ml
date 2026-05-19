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

module Sample_compare = struct
  type t = sample

  let repr = sample_repr

  include Repr.Poly (struct
      type nonrec t = t

      let repr = repr
    end)
end

module Tree_compare = struct
  type t = tree

  let repr = tree_repr

  include Repr.Poly (struct
      type nonrec t = t

      let repr = repr
    end)
end

let validate_comparable (type a) (repr : a Repr.t) =
  let module Compare = struct
    type t = a

    let repr = repr

    include Repr.Poly (struct
        type nonrec t = t

        let repr = repr
      end)
  end
  in
  ignore (Compare.equal, Compare.compare)
;;

let%expect_test "Poly accepts structural reprs" =
  let equal_sample = Sample_compare.equal in
  let compare_sample = Sample_compare.compare in
  let left =
    { label = "a"; score = Some 1; marks = [| 'x'; 'y' |]; tags = [ "m"; "n" ] }
  in
  let right =
    { label = "a"; score = Some 1; marks = [| 'x'; 'y' |]; tags = [ "m"; "n" ] }
  in
  let later = { label = "b"; score = None; marks = [| 'z' |]; tags = [ "m" ] } in
  print_endline (Bool.to_string (equal_sample left right));
  print_endline (Ordering.to_string (compare_sample left later));
  let equal_tree = Tree_compare.equal in
  let compare_tree = Tree_compare.compare in
  let tree = Node [ Leaf 1; Node [ Leaf 2 ] ] in
  print_endline (Bool.to_string (equal_tree tree tree));
  print_endline (Ordering.to_string (compare_tree (Leaf 1) (Node [ Leaf 1 ])));
  let module T4_compare = struct
    type t = int * string * bool * char

    let repr = Repr.T4.repr Repr.int Repr.string Repr.bool Repr.char

    include Repr.Poly (struct
        type nonrec t = t

        let repr = repr
      end)
  end
  in
  let equal_t4 = T4_compare.equal in
  let compare_t4 = T4_compare.compare in
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

let%expect_test "Poly rejects floats view and abstract reprs" =
  print_code_error (fun () -> validate_comparable Repr.float);
  print_code_error (fun () -> validate_comparable Repr.(option float));
  print_code_error (fun () ->
    validate_comparable (Repr.view Repr.string ~to_:string_of_int));
  print_code_error (fun () -> validate_comparable (Repr.abstract Dyn.int));
  [%expect
    {|
    ("Repr.Poly: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "float" })
    ("Repr.Poly: repr is not sound for polymorphic comparison",
     { path = [ "root"; "option" ]; repr_kind = "float" })
    ("Repr.Poly: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "view" })
    ("Repr.Poly: repr is not sound for polymorphic comparison",
     { path = [ "root" ]; repr_kind = "abstract" })
  |}]
;;
