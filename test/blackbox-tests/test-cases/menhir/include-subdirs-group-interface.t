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
  File "group/group__mock.ml.mock", line 1:
  Warning 63 [erroneous-printed-signature]: The printed interface differs from
    the inferred interface. The inferred interface contained items which could
    not be printed properly due to name collisions between identifiers.
    File "_none_", line 1:
    Definition of module Dune__exe__Group__/2 Beware
    that this warning is purely informational and will not catch all instances
    of erroneous printed interface.
