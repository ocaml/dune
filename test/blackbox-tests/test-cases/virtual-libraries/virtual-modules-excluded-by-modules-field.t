Specifying a virtual module that isn't inside the (modules ..) field:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir src
  $ cat > src/dune << EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (virtual_modules x)
  >  (modules y))
  > EOF

  $ touch src/x.mli

  $ cat > src/y.ml << EOF
  > module type F = X
  > EOF

  $ mkdir src/impl
  $ cat > src/impl/dune << EOF
  > (library
  >  (name impl)
  >  (implements foo))
  > EOF
  $ touch src/impl/x.ml

X is warned about:

  $ dune build
  File "src/dune", line 4, characters 18-19:
  4 |  (virtual_modules x)
                        ^
  Warning: These modules appear in the virtual_modules field:
  - X
  They must also appear in the modules field.
  File "src/y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  File "src/impl/dune", lines 1-3, characters 0-40:
  1 | (library
  2 |  (name impl)
  3 |  (implements foo))
  Error: No rule found for src/.foo.objs/y.impl.all-deps
  [1]

In 3.11 onwards this warning becomes an error

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

  $ dune build ./bar.exe
  File "src/dune", line 4, characters 18-19:
  4 |  (virtual_modules x)
                        ^
  Error: These modules appear in the virtual_modules field:
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
  Error: No implementation found for virtual library "foo" in
  _build/default/src.
  -> required by executable bar in dune:3
  -> required by _build/default/.bar.eobjs/byte/dune__exe__Bar.cmi
  -> required by _build/default/.bar.eobjs/native/dune__exe__Bar.cmx
  -> required by _build/default/bar.exe
  [1]
