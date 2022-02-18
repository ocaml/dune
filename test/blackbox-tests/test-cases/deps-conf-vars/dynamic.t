The following should fail saying that %{read:...} isn't allowed in this
position, because it was indeed not supported by older versions of Dune. Due to
improvements in the core of Dune, this is now supported. Because the error used
to be discovered dynamically as we were trying to evaluate the dependencies,
the new code simply doesn't detect it. We should add a proper version check for
this feature.

  $ echo "(lang dune 1.6)" > dune-project
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (echo bar)))
  > (alias
  >  (name default)
  >  (deps %{read:foo}))
  > EOF
  $ dune build
  Error: No rule found for bar
  -> required by alias default in dune:2
  [1]
