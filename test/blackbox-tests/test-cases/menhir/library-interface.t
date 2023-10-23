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

  $ touch m.ml

  $ cat >foo.mly <<EOF
  > %{
  > module M = M
  > %}
  > %token EOF
  > 
  > %start<unit> unit
  > %%
  > 
  > unit:
  > | EOF { () }
  > EOF

  $ dune build foo.cma
