A copy action on a directory dependency should produce regular files in every
sandbox mode. This case has no overlapping dependencies.

  $ export DUNE_CACHE=disabled
  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action
  >   (progn
  >    (run mkdir -p tree/nested/empty tree/empty)
  >    (system "echo contents > tree/file")
  >    (system "echo nested > tree/nested/file"))))
  > (rule
  >  (target (dir copied))
  >  (deps tree (sandbox always))
  >  (action (no-infer (copy tree copied))))
  > EOF

First check that the directory dependency contains a regular file.

  $ dune build tree/file
  $ dune_cmd stat kind _build/default/tree/file
  regular file

Symlink mode: the copy action follows the sandbox's file links and produces
regular output files.

  $ dune build copied --sandbox symlink
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file
  $ cat _build/default/copied/nested/file
  nested
  $ dune_cmd stat kind _build/default/copied/nested/empty
  directory
  $ dune_cmd stat kind _build/default/copied/empty
  directory

Copy mode produces a regular file with the expected contents.

  $ dune build copied --sandbox copy
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file
  $ cat _build/default/copied/nested/file
  nested
  $ dune_cmd stat kind _build/default/copied/nested/empty
  directory
  $ dune_cmd stat kind _build/default/copied/empty
  directory

Hardlink mode also produces a regular file with the expected contents.

  $ dune build copied --sandbox hardlink
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file
  $ cat _build/default/copied/nested/file
  nested
  $ dune_cmd stat kind _build/default/copied/nested/empty
  directory
  $ dune_cmd stat kind _build/default/copied/empty
  directory
