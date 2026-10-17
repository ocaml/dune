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

A symlink already present in the input directory is not a sandbox-created link.
Recursive copying rejects it even when its target is a regular file.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action (system "mkdir tree && echo contents > tree/file && ln -s file tree/link")))
  > (rule
  >  (target (dir copied))
  >  (deps tree (sandbox always))
  >  (action (no-infer (copy tree copied))))
  > EOF
  $ dune build tree/file
  $ dune_cmd stat kind _build/default/tree/link
  symbolic link
  $ dune build copied --sandbox symlink
  File "dune", lines 4-7, characters 0-99:
  4 | (rule
  5 |  (target (dir copied))
  6 |  (deps tree (sandbox always))
  7 |  (action (no-infer (copy tree copied))))
  Error: Symlink link is not allowed here
  [1]

Depending on the file directly creates a real sandbox directory containing a
file symlink. The copy action currently rejects this sandbox-created link too.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action (system "mkdir tree && echo contents > tree/file")))
  > (rule
  >  (target (dir copied))
  >  (deps tree/file (sandbox always))
  >  (action (no-infer (copy tree copied))))
  > EOF
  $ dune build copied --sandbox symlink > copy.log 2>&1
  [1]
  $ grep '^Error:' copy.log
  Error: Symlink file is not allowed here

A link replaced by the action must not be treated as a sandbox-created link.
Here it points at its own copy destination.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action (system "mkdir tree && echo contents > tree/file")))
  > (rule
  >  (target (dir copied))
  >  (deps tree/file (sandbox always))
  >  (action
  >   (no-infer
  >    (progn
  >     (system "rm tree/file && mkdir copied")
  >     (write-file copied/file sentinel)
  >     (system "ln -s ../copied/file tree/file")
  >     (copy tree copied)))))
  > EOF
  $ dune build copied --sandbox symlink > copy.log 2>&1
  [1]
  $ grep '^Error:' copy.log
  Error: Symlink file is not allowed here

External directory inputs are not populated in the sandbox. Their file links
must still be rejected, including links to files outside the input directory.

  $ mkdir -p external/tree project
  $ echo external > external/payload
  $ ln -s ../payload external/tree/link
  $ cd project
  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir copied))
  >  (deps ../external/tree (sandbox always))
  >  (action (no-infer (copy ../external/tree copied))))
  > EOF
  $ dune build copied --sandbox symlink > copy.log 2>&1
  [1]
  $ grep '^Error:' copy.log
  Error: Symlink link is not allowed here
