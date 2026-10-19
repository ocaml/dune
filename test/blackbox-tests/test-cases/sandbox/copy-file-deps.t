Relocating a sandboxed file dependency can change what its relative link
resolves to. Both the producer and the consumer declare ordinary file targets.

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
