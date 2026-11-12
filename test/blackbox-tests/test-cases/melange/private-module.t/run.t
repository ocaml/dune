Without sandboxing, OCaml resolves this wrapper alias.

  $ mv dune dune.melange
  $ cat > dune <<'EOF'
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (modes byte)
  >  (libraries lib))
  > EOF
  $ dune build --sandbox none foo.bc
  $ mv dune.melange dune

Melange currently fails to resolve the same wrapper alias.

  $ dune build --sandbox none private-module/foo.js
  File "foo.ml", line 1, characters 0-5:
  1 | Lib.X.run ();;
      ^^^^^
  Error: The module Lib.X is an alias for module Lib__X, which is missing
  [1]

The internal compilation-unit name remains unavailable.

  $ rm foo.ml
  $ echo 'Lib__X.run ();;' > foo.ml
  $ dune build --sandbox none private-module/foo.js
  File "foo.ml", line 1, characters 0-6:
  1 | Lib__X.run ();;
      ^^^^^^
  Error: Unbound module Lib__X
  [1]
