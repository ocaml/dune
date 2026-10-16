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

Symlink mode: the directory alone works, but adding an explicit file dependency
fails.

  $ dune build @directory --sandbox symlink
  contents
  $ dune build @overlap --sandbox symlink 2>&1 | censor
  Error:
  symlink(_build/.sandbox/$DIGEST/default/tree): File exists
  -> required by alias overlap in dune:8
  [1]

Copy mode: both builds succeed. This does not establish that overlapping inputs
are copied only once.

  $ dune build @directory --sandbox copy
  contents
  $ dune build @overlap --sandbox copy
  contents

Hardlink mode: a directory-only dependency already fails.

  $ dune build @directory --sandbox hardlink 2>&1 | censor
  Error:
  link(_build/.sandbox/$DIGEST/default/tree): Operation not permitted
  -> required by alias directory in dune:4
  [1]

With an overlapping file dependency, Dune tries to unlink the existing directory.

  $ dune build @overlap --sandbox hardlink 2>&1 | censor
  Error:
  unlink(_build/.sandbox/$DIGEST/default/tree): Is a directory
  -> required by alias overlap in dune:8
  [1]
