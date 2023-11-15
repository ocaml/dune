%{bin-available:...} should work without causing a dependency cycle in this
situation. See #9187.

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name p))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name e)
  >  (package p))
  > 
  > (library
  >  (package p)
  >  (name l)
  >  (modules))
  > 
  > (rule
  >  (enabled_if %{bin-available:e})
  >  (action
  >   (write-file aa x)))
  > EOF
  $ touch e.ml

  $ dune build
  Error: Dependency cycle between:
     Computing directory contents of _build/default
  -> Computing installable artifacts for package p
  -> %{bin-available:e} at dune:11
  -> Computing directory contents of _build/default
  [1]
