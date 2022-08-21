  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune build file @install
  File "dune-workspace", line 5, characters 9-65:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (targets foo)
  8 |  (host default)))
  Error: `targets` and `host` options cannot be used in the same context.
  [1]
