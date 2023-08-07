Demonstrate the behavior when a module is listed by private_modules by not by
modules:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir src
  $ cat > src/dune << EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (modules y)
  >  (private_modules x))
  > EOF

  $ cat > src/x.ml << EOF
  > let foo = ()
  > EOF

  $ cat > src/y.ml << EOF
  > let () = X.foo ()
  > EOF

X is warned about:

  $ dune build
  File "src/dune", line 5, characters 18-19:
  5 |  (private_modules x))
                        ^
  Warning: These modules appear in the private_modules field:
  - X
  They must also appear in the modules field.
  File "src/y.ml", line 1, characters 9-14:
  1 | let () = X.foo ()
               ^^^^^
  Error: Unbound module X
  [1]

In 3.11 onwards this warning becomes an error

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

  $ dune build
  File "src/dune", line 5, characters 18-19:
  5 |  (private_modules x))
                        ^
  Error: These modules appear in the private_modules field:
  - X
  They must also appear in the modules field.
  [1]

This warning should be ignored if we are in vendored_dirs

  $ cat > dune << EOF
  > (vendored_dirs src)
  > (executable
  >  (name bar)
  >  (libraries foo))
  > EOF
  $ cat > bar.ml

  $ dune build ./bar.exe
  File "src/y.ml", line 1, characters 9-14:
  1 | let () = X.foo ()
               ^^^^^
  Error: Unbound module X
  [1]
