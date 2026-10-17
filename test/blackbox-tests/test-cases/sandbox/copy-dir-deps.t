A copy action on a directory dependency should produce regular files in every
sandbox mode. This case has no overlapping dependencies.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action (system "mkdir tree && echo contents > tree/file")))
  > (rule
  >  (target (dir copied))
  >  (deps tree (sandbox always))
  >  (action (no-infer (copy tree copied))))
  > EOF

First check that the directory dependency contains a regular file.

  $ dune build tree/file
  $ dune_cmd stat kind _build/default/tree/file
  regular file

Symlink mode: the copy action follows the directory link and copies its files.

  $ dune build copied --sandbox symlink
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file

Copy mode also produces a regular file with the expected contents.

  $ dune build copied --sandbox copy
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file

Hardlink mode currently fails while populating the directory dependency,
before the copy action runs.

  $ dune build copied --sandbox hardlink 2>&1 | censor
  Error:
  link(_build/.sandbox/$DIGEST/default/tree): Operation not permitted
  -> required by _build/default/copied
  [1]
