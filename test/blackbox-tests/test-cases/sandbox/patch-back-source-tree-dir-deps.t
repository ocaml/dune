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

CR: Patch-back sandbox population currently tries to copy the directory as a
single file, so the action never runs.

  $ dune build copied
  Error: Is a directory
  -> required by _build/default/copied
  [1]

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

CR: Editing a generated input currently produces an ordinary diff, and unrelated
source changes from the same action are still registered for promotion.

  $ cat > edit.sh <<'EOF'
  > echo source-change > aaa-source
  > echo changed > tree/sub/file
  > EOF
  $ dune build @edit
  File "aaa-source", line 1, characters 0-0:
  --- aaa-source
  +++ _build/default/aaa-source
  @@ -0,0 +1 @@
  +source-change
  File "tree/sub/file", line 1, characters 0-0:
  --- tree/sub/file
  +++ _build/default/tree/sub/file
  @@ -0,0 +1 @@
  +changed
  [1]
  $ dune promotion list
  aaa-source
  $ test ! -e _build/.to-promote
  [1]

CR: A new file inside a directory target is incorrectly offered for promotion.

  $ echo 'echo added > tree/sub/added' > edit.sh
  $ dune build @edit
  File "tree/sub/added", line 1, characters 0-0:
  --- tree/sub/added
  +++ _build/default/tree/sub/added
  @@ -0,0 +1 @@
  +added
  [1]
  $ dune promotion list
  tree/sub/added
  $ test ! -e _build/.to-promote
  [1]

CR: Deleting a generated input incorrectly registers a source-tree deletion.

  $ echo 'rm tree/sub/file' > edit.sh
  $ dune build @edit
  File "dune", lines 17-20, characters 0-110:
  17 | (rule
  18 |  (alias edit)
  19 |  (deps tree/sub/file edit.sh (sandbox patch_back_source_tree))
  20 |  (action (run sh edit.sh)))
  Error: File tree/sub/file should be deleted
  [1]
  $ dune promotion list
  $ test ! -e _build/.to-promote
  [1]

CR: Creating an empty directory inside a directory target goes unnoticed.

  $ echo 'mkdir tree/new-empty' > edit.sh
  $ dune build @edit
  $ dune promotion list
  $ test ! -e _build/.to-promote

The generated inputs outside the sandbox remain unchanged.

  $ cat _build/default/tree/sub/file
  contents
  $ test ! -e _build/default/tree/sub/added
  $ test ! -e _build/default/tree/new-empty
  $ test ! -e tree
