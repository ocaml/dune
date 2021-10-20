  $ dune build --root static
  Entering directory 'static'
  deps: ../install/default/lib/foo/foo.cma

The following should fail saying that %{read:...} isn't allowed in this
position, because it was indeed not supported by older versions of Dune. Due to
improvements in the core of Dune, this is now supported. Because the error used
to be discovered dynamically as we were trying to evaluate the dependencies,
the new code simply doesn't detect it. We should add a proper version check for
this feature.

  $ dune build --root dynamic
  Entering directory 'dynamic'
  Error: No rule found for foo
  -> required by %{read:foo} at dune:3
  -> required by alias default in dune:1
  [1]

  $ dune build --root alias-lib-file
  Entering directory 'alias-lib-file'
  deps: ../install/default/lib/foo/theories/a
