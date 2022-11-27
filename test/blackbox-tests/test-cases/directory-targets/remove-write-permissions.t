Write permissions on directory targets.

  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action (system "mkdir -p foo/foo2 && touch foo/foo2/bar"))
  >  (targets (dir foo)))
  > EOF

  $ dune build ./foo
  $ dir=_build/default/foo
  $ dune_cmd stat permissions $dir
  755
  $ dune_cmd stat permissions $dir/foo2
  755
  $ dune_cmd stat permissions $dir/foo2/bar
  444
