We should be able to use menhir as a library interface:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using menhir 2.1)
  > EOF

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
