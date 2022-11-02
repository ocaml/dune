Glob deps in a subdirectory that refer to a path outside that subdirectory

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > EOF

  $ mkdir -p foo bar

  $ cat > foo/dune <<EOF
  > (rule
  >  (alias x)
  >  (deps (glob_files ../bar/*.txt))
  >  (action (system "for i in %{deps}; do printf \"\$i\\n\"; done")))
  > EOF

  $ touch bar/a.txt bar/b.txt

  $ dune build @foo/x
  ../bar/a.txt
  ../bar/b.txt
