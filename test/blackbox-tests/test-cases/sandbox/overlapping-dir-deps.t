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

Symlink mode: both dependencies expose the expected contents. This does not
establish that overlapping files are populated only once.

  $ dune build @directory --sandbox symlink
  contents
  $ dune build @overlap --sandbox symlink
  contents

Copy mode: reject the second attempt to populate an already copied file.
This exposes the overlap rather than silently overwriting its contents. Normalize
both syscall errors and the portable copier's wrapped diagnostic.

  $ dune build @directory --sandbox copy
  contents
  $ dune build @overlap --sandbox copy > copy.log 2>&1
  [1]
  $ sed -E \
  >   -e '/^Error.*:$/ { N; s/\n/ /; }' \
  >   -e 's/(open|copyfile)\((.*)\): File exists$/\2: File exists/' \
  >   copy.log | censor
  Error: _build/.sandbox/$DIGEST/default/tree/file: File exists
  -> required by alias overlap in dune:8

Hardlink mode: both dependencies also expose the expected contents.

  $ dune build @directory --sandbox hardlink
  contents
  $ dune build @overlap --sandbox hardlink
  contents
