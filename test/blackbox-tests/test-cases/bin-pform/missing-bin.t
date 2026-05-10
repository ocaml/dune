A %{bin:NAME} pform (in action or deps) and a (run LITERAL) action
where the name doesn't resolve all fail with "Program ... not found".
Three different code paths converge on the same error, and each is
exercised so any one can regress independently.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > EOF

In an action:

  $ cat >dune <<'EOF'
  > (rule (with-stdout-to dummy (echo "%{bin:fakebin}")))
  > EOF
  $ dune build ./dummy
  File "dune", line 1, characters 35-49:
  1 | (rule (with-stdout-to dummy (echo "%{bin:fakebin}")))
                                         ^^^^^^^^^^^^^^
  Error: Program fakebin not found in the tree or in PATH
   (context: default)
  [1]

In the deps field:

  $ cat >dune <<'EOF'
  > (rule (deps %{bin:foobar}) (target dummy) (action (with-stdout-to %{target} (echo foo))))
  > EOF
  $ dune build ./dummy
  File "dune", line 1, characters 12-25:
  1 | (rule (deps %{bin:foobar}) (target dummy) (action (with-stdout-to %{target} (echo foo))))
                  ^^^^^^^^^^^^^
  Error: Program foobar not found in the tree or in PATH
   (context: default)
  [1]

In a (run LITERAL) action:

  $ cat >dune <<'EOF'
  > (rule (with-stdout-to dummy (run nopebin)))
  > EOF
  $ dune build ./dummy
  File "dune", line 1, characters 33-40:
  1 | (rule (with-stdout-to dummy (run nopebin)))
                                       ^^^^^^^
  Error: Program nopebin not found in the tree or in PATH
   (context: default)
  [1]
