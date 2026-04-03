Depend on a source directory.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir foo
  $ touch foo/{x,y,z}

  $ cat >dune <<EOF
  > (rule
  >  (deps foo)
  >  (target bar)
  >  (action (bash "find %{deps} -mindepth 1 | sed 's#.*foo/##' | sort > %{target}")))
  > EOF

  $ dune build ./bar

  $ cat _build/default/bar
  x
  y
  z
