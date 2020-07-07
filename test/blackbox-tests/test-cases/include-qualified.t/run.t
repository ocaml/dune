Basic test showcasing the feature. Every directory creates a new level of aliasing.
  $ dune build --root basic
  Entering directory 'basic'
  File "lib/dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  [1]

We are also allowed to write lib interface files at each level.
  $ dune build --root nested-lib-interface
  Entering directory 'nested-lib-interface'
  File "lib/dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  [1]

We can nested modules virtual
  $ dune build @all --root nested-virtual
  Entering directory 'nested-virtual'
  File "impl/dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  [1]

We can set preprocessing options for nested modules
  $ dune build @all --root pp
  Entering directory 'pp'
  File "dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  [1]
