When (lang dune unstable) is present, unstable features are available.

By default, this is not available:

  $ dune printenv --root unstable
  File "dune-project", line 1, characters 11-19:
  Error: Version unstable of dune is not supported.
  Supported versions:
  - 0.0
  - 1.0 to 1.1
  [1]

Passing --unstable unlocks this:

  $ dune printenv --unstable --root unstable
  Entering directory 'unstable'
  (
   (flags
    (-w
     @a-4-29-40-41-42-44-45-48-58-59-60-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs))
   (ocamlc_flags (-g))
   (ocamlopt_flags (-g))
  )
