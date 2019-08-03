  $ dune build 2>&1 | grep -v ocamlc
  File "fooexe.ml", line 3, characters 0-7:
  3 | Bar.run ();;
      ^^^^^^^
  Error (alert deprecated): module Bar
  Will be removed past 2020-20-20. Use Mylib.Bar instead.
  File "fooexe.ml", line 4, characters 0-7:
  4 | Foo.run ();;
      ^^^^^^^
  Error (alert deprecated): module Foo
  Will be removed past 2020-20-20. Use Mylib.Foo instead.
  File "fooexe.ml", line 7, characters 11-22:
  7 | module X : Intf_only.S = struct end
                 ^^^^^^^^^^^
  Error (alert deprecated): module Intf_only
  Will be removed past 2020-20-20. Use Mylib.Intf_only instead.
