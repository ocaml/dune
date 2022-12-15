We should forbid lib interfaces modules from depending on themselves:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = Foo.f ()
  > EOF

  $ touch bar.ml

  $ dune build @check
  File "foo.ml", line 1, characters 9-14:
  1 | let () = Foo.f ()
               ^^^^^
  Error: Unbound module Foo
  [1]
