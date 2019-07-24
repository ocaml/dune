Implementation of library from another project is not allowed when tagged with
variant.

  $ dune build
  File "prj2/dune", line 4, characters 13-20:
  4 |  (implements vlibfoo)
                   ^^^^^^^
  Error: Virtual library "vlibfoo" does not know about implementation "impl"
  with variant "somevariant". Instead of using (variant somevariant) here, you
  need to reference it in the virtual library project, using the
  external_variant stanza:
  (external_variant
    (virtual_library vlibfoo)
    (variant somevariant)
    (implementation impl))
  [1]
