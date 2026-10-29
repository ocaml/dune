Relocating a sandboxed file dependency can change what its relative link
resolves to. Both the producer and the consumer declare ordinary file targets.

  $ export DUNE_CACHE=disabled
  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (subdir tree
  >  (rule
  >   (target file)
  >   (action (system "echo contents > file"))))
  > (rule
  >  (target copied)
  >  (deps tree/file (sandbox always))
  >  (action
  >   (progn
  >    (system "mkdir -p a/b/c default/tree")
  >    (system "echo wrong > default/tree/file")
  >    (system "mv tree a/b/c/tree")
  >    (system "ln -s a/b/c/tree tree")
  >    (copy tree/file copied))))
  > EOF

The generated input is a regular file.

  $ dune build tree/file
  $ dune_cmd stat kind _build/default/tree/file
  regular file

Symlink mode copies the action-created file instead of the original input.

  $ dune build copied --sandbox symlink
  $ cat _build/default/copied
  wrong
  $ dune_cmd stat kind _build/default/copied
  regular file

Copy mode produces a regular file with the expected contents.

  $ dune build copied --sandbox copy
  $ cat _build/default/copied
  contents
  $ dune_cmd stat kind _build/default/copied
  regular file

Hardlink mode also produces a regular file with the expected contents.

  $ dune build copied --sandbox hardlink
  $ cat _build/default/copied
  contents
  $ dune_cmd stat kind _build/default/copied
  regular file

A directory target may be populated by copying a file into a nested path.
The copy action must create the intermediate directories.

  $ echo nested > input
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir output))
  >  (deps input (sandbox always))
  >  (action (no-infer (copy input output/nested/file))))
  > EOF
  $ dune build output --sandbox copy
  $ cat _build/default/output/nested/file
  nested

The traversal entrypoint must also prepare the parent of a file root.
Here the destination basename matches the declared directory target.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target (dir same-name))
  >  (deps input (sandbox always))
  >  (action (no-infer (copy input same-name/nested/same-name))))
  > EOF
  $ dune build same-name --sandbox copy
  $ cat _build/default/same-name/nested/same-name
  nested
