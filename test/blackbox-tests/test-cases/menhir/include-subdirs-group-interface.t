We should be able to use menhir as a group interface:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using menhir 2.1)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable (name foo))
  > EOF
  $ touch foo.ml

  $ mkdir group
  $ cat >group/dune <<EOF
  > (menhir (modules group))
  > EOF

  $ cat >group/group.mly <<EOF
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

  $ touch group/m.ml

  $ dune build
  File "group/group.mly", line 2, characters 11-12:
  Error: Unbound module M
  [1]
