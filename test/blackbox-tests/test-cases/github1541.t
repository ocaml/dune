This demonstrates error messages whennever a pform isn't expanded

  $ echo '(lang dune 2.0)' > dune-project

for libraries:

  $ echo '(rule (with-stdout-to dummy (echo "%{lib:fakelib:bar.ml}")))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 35-56:
  1 | (rule (with-stdout-to dummy (echo "%{lib:fakelib:bar.ml}")))
                                         ^^^^^^^^^^^^^^^^^^^^^
  Error: Library "fakelib" not found.
  -> required by %{lib:fakelib:bar.ml} at dune:1
  -> required by _build/default/dummy
  [1]

for binaries:

  $ echo '(rule (with-stdout-to dummy (echo "%{bin:fakebin}")))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 35-49:
  1 | (rule (with-stdout-to dummy (echo "%{bin:fakebin}")))
                                         ^^^^^^^^^^^^^^
  Error: Program fakebin not found in the tree or in PATH
   (context: default)
  [1]

for libraries in the deps field:

  $ echo '(rule (deps %{lib:fakelib:bar.ml}) (target dummy) (action (with-stdout-to %{target} (echo foo))))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 12-33:
  1 | (rule (deps %{lib:fakelib:bar.ml}) (target dummy) (action (with-stdout-to %{target} (echo foo))))
                  ^^^^^^^^^^^^^^^^^^^^^
  Error: Library "fakelib" not found.
  -> required by %{lib:fakelib:bar.ml} at dune:1
  -> required by _build/default/dummy
  [1]

for binaries in the deps field:

  $ echo '(rule (deps %{bin:foobar}) (target dummy) (action (with-stdout-to %{target} (echo foo))))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 12-25:
  1 | (rule (deps %{bin:foobar}) (target dummy) (action (with-stdout-to %{target} (echo foo))))
                  ^^^^^^^^^^^^^
  Error: Program foobar not found in the tree or in PATH
   (context: default)
  [1]
