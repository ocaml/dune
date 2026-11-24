Module artifact variables should find modules owned by melange.emit stanzas.

  $ make_melange_project 3.25 1.0
  $ cat > dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name lib)
  >  (modules lib)
  >  (modes melange))
  > (melange.emit
  >  (target output)
  >  (modules main sub.nested))
  > (melange.emit
  >  (target disabled)
  >  (modules main)
  >  (enabled_if false))
  > EOF
  $ echo 'let value = 1' > lib.ml
  $ echo 'let value = 2' > main.ml
  $ echo 'val value : int' > main.mli
  $ mkdir sub
  $ echo 'let value = 3' > sub/nested.ml

The emit artifacts exist, but module artifact lookup cannot find them.
The disjoint library module remains available.

  $ dune build .output.mobjs/melange/melange__Main.{cmi,cmt,cmti}
  $ dune build '%{cmi:lib}'
  $ dune build '%{cmi:main}'
  File "command line", line 1, characters 0-11:
  Error: Module Main does not exist.
  [1]
  $ dune build '%{cmt:main}'
  File "command line", line 1, characters 0-11:
  Error: Module Main does not exist.
  [1]
  $ dune build '%{cmti:main}'
  File "command line", line 1, characters 0-12:
  Error: Module Main does not exist.
  [1]
  $ dune build '%{cmi:sub.nested}'
  File "command line", line 1, characters 0-17:
  Error: Module Sub.Nested does not exist.
  [1]

Multiple emit stanzas selecting the same module must not crash artifact lookup.

  $ cat > dune <<'EOF'
  > (melange.emit
  >  (target first)
  >  (modules main))
  > (melange.emit
  >  (target second)
  >  (modules main))
  > EOF
  $ dune build '%{cmi:main}'
  File "command line", line 1, characters 0-11:
  Error: Module Main does not exist.
  [1]
