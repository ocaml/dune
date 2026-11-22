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

Module artifact lookup finds emit-owned modules alongside disjoint library
modules, without including disabled emit stanzas.

  $ dune build .output.mobjs/melange/melange__Main.{cmi,cmt,cmti}
  $ dune build '%{cmi:lib}'
  $ dune build '%{cmi:main}'
  $ dune build '%{cmt:main}'
  $ dune build '%{cmti:main}'
  $ dune build '%{cmi:sub.nested}'

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
  Error: Module reference Main is ambiguous.
  [1]
