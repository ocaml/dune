We should be able to use menhir as a library interface:

  $ make_menhir_project 3.11 2.1

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > (menhir (modules foo))
  > EOF

  $ cat > m.ml << EOF
  > let x = ()
  > EOF

  $ cat >foo.mly <<EOF
  > %token EOF
  > 
  > %start<unit> unit
  > %%
  > 
  > unit:
  > | EOF { M.x }
  > EOF

  $ dune build foo.cma
