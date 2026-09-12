open Base
open Base_quickcheck.Export
module Al = Stdune.Appendable_list

module Input = struct
  type t =
    | Empty
    | Singleton of int
    | Cons of int * t
    | Of_list of int list
    | Append of t * t
    | Concat of t list
  [@@deriving sexp_of, quickcheck]

  let rec to_appendable_list = function
    | Empty -> Al.empty
    | Singleton x -> Al.singleton x
    | Cons (x, xs) -> Al.cons x (to_appendable_list xs)
    | Of_list xs -> Al.of_list xs
    | Append (xs, ys) -> Al.(to_appendable_list xs @ to_appendable_list ys)
    | Concat xs -> Al.concat (List.map xs ~f:to_appendable_list)
  ;;

  let rec to_list = function
    | Empty -> []
    | Singleton x -> [ x ]
    | Cons (x, xs) -> x :: to_list xs
    | Of_list xs -> xs
    | Append (xs, ys) -> List.append (to_list xs) (to_list ys)
    | Concat xs -> List.concat_map xs ~f:to_list
  ;;
end

let quick_test_config =
  { Base_quickcheck.Test.default_config with
    test_count = 200
  ; sizes = Sequence.cycle_list_exn (List.init 11 ~f:Fn.id)
  }
;;

let%quick_test ("traversals agree with lists" [@config quick_test_config]) =
  fun (input : Input.t) ->
  let xs = Input.to_appendable_list input in
  let expected = Input.to_list input in
  assert (Al.length xs = List.length expected);
  assert (Bool.equal (Al.is_empty xs) (List.is_empty expected));
  assert (List.equal Int.equal (Al.to_list xs) expected);
  assert (List.equal Int.equal (Al.to_list_rev xs) (List.rev expected));
  assert (
    List.equal
      Int.equal
      (Stdune.Array.Immutable.to_list (Al.to_immutable_array xs))
      expected);
  let visited = ref [] in
  Al.iter xs ~f:(fun x -> visited := x :: !visited);
  assert (List.equal Int.equal (List.rev !visited) expected)
;;

let%quick_test ("map agrees with List.map" [@config quick_test_config]) =
  fun (input : Input.t) (f : int -> int) ->
  let actual = Al.map (Input.to_appendable_list input) ~f |> Al.to_list in
  let expected = List.map (Input.to_list input) ~f in
  assert (List.equal Int.equal actual expected)
;;

let%quick_test ("exists agrees with List.exists" [@config quick_test_config]) =
  fun (input : Input.t) (f : int -> bool) ->
  let actual = Al.exists (Input.to_appendable_list input) ~f in
  let expected = List.exists (Input.to_list input) ~f in
  assert (Bool.equal actual expected)
;;
