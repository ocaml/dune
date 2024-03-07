Specifying a module without implementation that isn't inside the (modules ..)
field

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir src
  $ cat > src/dune << EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (modules_without_implementation x)
  >  (modules y))
  > EOF

  $ touch src/x.mli

  $ cat > src/y.ml << EOF
  > module type F = X
  > EOF

X is warned about:

  $ dune build
  File "src/dune", line 4, characters 33-34:
  4 |  (modules_without_implementation x)
                                       ^
  Warning: These modules appear in the modules_without_implementation field:
  - X
  They must also appear in the modules field.
  File "src/y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  [1]

In 3.11 onwards this warning becomes an error

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

  $ dune build
  File "src/dune", line 4, characters 33-34:
  4 |  (modules_without_implementation x)
                                       ^
  Error: These modules appear in the modules_without_implementation field:
  - X
  They must also appear in the modules field.
  [1]

This should be ignored if we are in vendored_dirs

  $ cat > dune << EOF
  > (vendored_dirs src)
  > (executable
  >  (name bar)
  >  (libraries foo))
  > EOF
  $ cat > bar.ml

  $ dune build ./bar.exe
  File "src/y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  [1]
