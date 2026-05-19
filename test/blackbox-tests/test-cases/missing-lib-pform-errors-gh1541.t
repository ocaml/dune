A %{lib:LIB:FILE} pform where LIB doesn't resolve fails with
"Library ... not found", whether the pform appears in the action or
in the deps field.

  $ echo '(lang dune 2.0)' > dune-project

In an action:

  $ echo '(rule (with-stdout-to dummy (echo "%{lib:fakelib:bar.ml}")))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 35-56:
  1 | (rule (with-stdout-to dummy (echo "%{lib:fakelib:bar.ml}")))
                                         ^^^^^^^^^^^^^^^^^^^^^
  Error: Library "fakelib" not found.
  -> required by %{lib:fakelib:bar.ml} at dune:1
  -> required by _build/default/dummy
  [1]

In the deps field:

  $ echo '(rule (deps %{lib:fakelib:bar.ml}) (target dummy) (action (with-stdout-to %{target} (echo foo))))' > dune
  $ dune build ./dummy
  File "dune", line 1, characters 12-33:
  1 | (rule (deps %{lib:fakelib:bar.ml}) (target dummy) (action (with-stdout-to %{target} (echo foo))))
                  ^^^^^^^^^^^^^^^^^^^^^
  Error: Library "fakelib" not found.
  -> required by %{lib:fakelib:bar.ml} at dune:1
  -> required by _build/default/dummy
  [1]
