  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (alias y)
  >  (targets (dir foo))
  >  (action (progn
  >    (bash "mkdir -p foo/a/b1/c")
  >    (bash "mkdir -p foo/a/b2/c")
  >    (bash "mkdir -p foo/a/b3/c")
  >    (bash "touch foo/x.txt")
  >    (bash "touch foo/a/x.txt")
  >    (bash "touch foo/a/b1/c/x.txt")
  >    (bash "touch foo/a/b1/c/y.txt")
  >    (bash "touch foo/a/b3/x.txt")
  >    (bash "touch foo/a/b3/x.other") )
  >  )
  > )
  > (rule
  >  (alias x)
  >  (deps (glob_files_rec foo/*.txt))
  >  (action (bash "for i in %{deps}; do printf \"\$i\\n\"; done")))
  > EOF

  $ dune build @x
  foo/x.txt
