Package dependencies must materialize source-tree installs as well as files.

  $ make_dune_project_with_package 3.24 treepkg
  $ mkdir -p assets/sub
  $ echo tree > assets/sub/data.txt
  $ echo control > control.txt
  $ cat >dune <<'EOF'
  > (install
  >  (section share)
  >  (files control.txt)
  >  (source_trees assets))
  > (rule
  >  (target out)
  >  (deps (package treepkg))
  >  (action (write-file out staged)))
  > EOF

  $ dune build out
  $ layout=$(dirname \
  >   _build/_private/default/.packages/*/share/treepkg/control.txt)
  $ cat "$layout/control.txt"
  control
  $ test -f "$layout/assets/sub/data.txt"
  [1]

The same source tree is available through the flat installation.

  $ test ! -e _build/install/default/share/treepkg/assets/sub/data.txt
  $ dune build @install
  $ cat _build/install/default/share/treepkg/assets/sub/data.txt
  tree
