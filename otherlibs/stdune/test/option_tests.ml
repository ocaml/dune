open Stdune

let%expect_test "Option.hash: None <> Some x when (f x = 0)" =
  (* Bool.hash false = 0 and Unit.hash () = 0. Under the previous
     implementation [Option.hash f (Some x)] returned
     [Stdlib.Hashtbl.hash (f x)] and [Option.hash f None] returned
     [Stdlib.Hashtbl.hash None]; since [None] shares OCaml's tag-0
     representation with the integer 0, both produced the same hash
     whenever [f x = 0]. *)
  Printf.printf
    "Bool.hash false: %b\n"
    (Option.hash Bool.hash (Some false) <> Option.hash Bool.hash None);
  Printf.printf
    "Unit.hash ():    %b\n"
    (Option.hash Unit.hash (Some ()) <> Option.hash Unit.hash None);
  [%expect
    {|
    Bool.hash false: true
    Unit.hash ():    true
    |}]
;;
