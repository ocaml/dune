A dependency on a directory and an explicit dependency on one of its files
should expose the same contents as the directory dependency alone, in every
sandbox mode.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action (system "mkdir tree && echo contents > tree/file")))
  > (rule
  >  (alias directory)
  >  (deps tree (sandbox always))
  >  (action (run cat tree/file)))
  > (rule
  >  (alias overlap)
  >  (deps tree tree/file (sandbox always))
  >  (action (run cat tree/file)))
  > EOF

First check that the directory target produces the expected input.

  $ dune build tree/file
  $ cat _build/default/tree/file
  contents

Symlink mode: both dependencies expose the expected contents.

  $ dune build @directory --sandbox symlink
  contents
  $ dune build @overlap --sandbox symlink
  contents

Copy mode also accepts overlapping dependencies without overwriting files.

  $ dune build @directory --sandbox copy
  contents
  $ dune build @overlap --sandbox copy
  contents

Hardlink mode: both dependencies also expose the expected contents.

  $ dune build @directory --sandbox hardlink
  contents
  $ dune build @overlap --sandbox hardlink
  contents

Nested directory and file dependencies must also be populated only once.
A sibling whose name shares the directory's prefix is still a separate input.
The empty directory must remain available too.

  $ echo sibling > outer-other
  $ cat >> dune <<'EOF'
  > (rule
  >  (target (dir outer))
  >  (action
  >   (progn
  >    (run mkdir -p outer/inner/empty)
  >    (system "echo nested > outer/inner/file"))))
  > (rule
  >  (alias nested-overlap)
  >  (deps outer/inner/file outer-other outer/inner outer (sandbox always))
  >  (action
  >   (progn
  >    (run cat outer/inner/file outer-other)
  >    (run test -d outer/inner/empty))))
  > EOF

  $ dune build @nested-overlap --sandbox symlink
  nested
  sibling
  $ dune build @nested-overlap --sandbox hardlink
  nested
  sibling

  $ dune build @nested-overlap --sandbox copy
  nested
  sibling
