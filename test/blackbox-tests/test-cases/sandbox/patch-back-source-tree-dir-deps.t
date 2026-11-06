A generated directory dependency should be copied into a patch-back sandbox.
Its files and directories must be writable, including when the originals are
read-only, and empty directories must be preserved.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir tree))
  >  (action
  >   (progn
  >    (run mkdir -p tree/sub tree/empty)
  >    (system "echo contents > tree/sub/file")
  >    (run chmod a-w tree/sub/file tree/sub tree/empty))))
  > (rule
  >  (target copied)
  >  (deps tree (sandbox patch_back_source_tree))
  >  (action
  >   (progn
  >    (run test -d tree/empty)
  >    (run test -w tree/sub)
  >    (run test -w tree/sub/file)
  >    (no-infer (copy tree/sub/file copied)))))
  > EOF

The directory is the only declared input. Inferring a dependency on its file
would introduce a separate dependency-overlap problem.

  $ dune build tree/sub/file
  $ cat _build/default/tree/sub/file
  contents
  $ dune_cmd stat kind _build/default/tree/sub/file
  regular file

The action runs with a writable copy of the directory dependency.

  $ dune build copied
  $ cat _build/default/copied
  contents
  $ dune_cmd stat kind _build/default/copied
  regular file

Neither the declared output nor the untouched directory dependency should be
promoted into the source tree.

  $ dune promotion list
  $ test ! -e copied

Reading an individual file from a directory target is allowed too. Replacing it
with identical contents must not be mistaken for a patch-back change.

  $ cat >> dune <<'EOF'
  > (rule
  >  (alias edit)
  >  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  >  (action (run sh edit.sh)))
  > EOF
  $ cat > edit.sh <<'EOF'
  > cp tree/sub/file replacement
  > mv replacement tree/sub/file
  > EOF
  $ dune build @edit
  $ dune promotion list

Editing a generated input must be rejected before any promotions are registered,
including unrelated source changes from the same action.

  $ cat > edit.sh <<'EOF'
  > echo source-change > aaa-source
  > echo changed > tree/sub/file
  > EOF
  $ dune build @edit
  File "dune", lines 17-20, characters 0-110:
  17 | (rule
  18 |  (alias edit)
  19 |  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  20 |  (action (run sh edit.sh)))
  Error: Cannot patch back changes to tree/sub/file because it is part of a
  directory target.
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote

New files inside a directory target must be rejected too.

  $ echo 'echo added > tree/sub/added' > edit.sh
  $ dune build @edit
  File "dune", lines 17-20, characters 0-110:
  17 | (rule
  18 |  (alias edit)
  19 |  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  20 |  (action (run sh edit.sh)))
  Error: Cannot patch back changes to tree/sub/added because it is part of a
  directory target.
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote

Deleting a generated input must not register a source-tree deletion.

  $ echo 'rm tree/sub/file' > edit.sh
  $ dune build @edit
  File "dune", lines 17-20, characters 0-110:
  17 | (rule
  18 |  (alias edit)
  19 |  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  20 |  (action (run sh edit.sh)))
  Error: Cannot patch back changes to tree/sub/file because it is part of a
  directory target.
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote

Creating an empty directory inside a directory target must also be rejected.

  $ echo 'mkdir tree/new-empty' > edit.sh
  $ dune build @edit
  File "dune", lines 17-20, characters 0-110:
  17 | (rule
  18 |  (alias edit)
  19 |  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  20 |  (action (run sh edit.sh)))
  Error: Cannot patch back changes to tree/new-empty because it is part of a
  directory target.
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote

The generated inputs outside the sandbox remain unchanged.

  $ cat _build/default/tree/sub/file
  contents
  $ test ! -e _build/default/tree/sub/added
  $ test ! -e _build/default/tree/new-empty
  $ test ! -e tree
