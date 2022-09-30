When (name) points to a module that is not part of (modules), a specific error
message is printed.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ touch a.ml b.ml

  $ cat > dune << EOF
  > (executable
  >  (name a)
  >  (modules b))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-8:
  2 |  (name a)
             ^
  Error: The name "A" is not listed in the (modules) field of this stanza.
  [1]

This does not happen when (modules) is implicit.

  $ rm a.ml b.ml

  $ cat > dune << EOF
  > (executable
  >  (name a))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-8:
  2 |  (name a))
             ^
  Error: Module "A" doesn't exist.
  [1]

When the module is listed but the file does not exist, we get the "normal"
message.

  $ cat > dune << EOF
  > (executable
  >  (name a)
  >  (modules a))
  > EOF

  $ dune build
  File "dune", line 3, characters 10-11:
  3 |  (modules a))
                ^
  Error: Module A doesn't exist.
  [1]
