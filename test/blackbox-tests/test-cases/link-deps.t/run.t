It is possible to add link-time dependencies.

In particular, these can depend on the result of the compilation (like a .cmo
file) and be created just before linking.

  $ dune build link_deps.exe
  File "dune", line 7, characters 7-16:
  7 |  (name link_deps)
             ^^^^^^^^^
  Error: No rule found for .link_deps.eobjs/.byte_objs/link_deps.cmo
  [1]
