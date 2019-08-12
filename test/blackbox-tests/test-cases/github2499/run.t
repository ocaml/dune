Reproduction case for #2499: dune doesn't cleanup stale directories

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >dune <<EOF
  > (data_only_dirs data)
  > (rule
  >  (deps (source_tree data))
  >  (action (with-stdout-to list (system "find data -type f | sort"))))
  > EOF

  $ mkdir -p data/a data/b; touch data/a/x data/b/x

  $ dune build list
  $ cat _build/default/list
  data/a/x
  data/b/x

  $ rm -rf data/b

  $ dune build list
  $ cat _build/default/list
  data/a/x
