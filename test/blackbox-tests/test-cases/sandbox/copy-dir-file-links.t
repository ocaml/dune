Recursive copying of individual file dependencies across sandbox modes.
The later cases distinguish original input links from action-created links.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (subdir tree
  >  (rule
  >   (target file)
  >   (action (system "echo contents > file")))
  >  (rule
  >   (target link)
  >   (deps file)
  >   (action (run ln -s file link))))
  > (rule
  >  (target (dir copied))
  >  (deps tree/file (sandbox always))
  >  (action (no-infer (copy tree copied))))
  > (rule
  >  (target (dir copied-link))
  >  (deps tree/link (sandbox always))
  >  (action (no-infer (copy tree copied-link))))
  > (rule
  >  (target (dir copied-local))
  >  (deps tree/file (sandbox always))
  >  (action
  >   (no-infer
  >    (progn
  >     (system "mkdir -p a/b/c/tree default/tree")
  >     (system "echo local > default/tree/file")
  >     (system "ln -s ../../../../default/tree/file a/b/c/tree/alias")
  >     (copy a/b/c/tree copied-local)))))
  > EOF

Copy mode produces a regular file with the expected contents.

  $ dune build copied --sandbox copy
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file

Hardlink mode also produces a regular file with the expected contents.

  $ dune build copied --sandbox hardlink
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file

Symlink mode copies the sandbox link into a regular file too.

  $ dune build copied --sandbox symlink
  $ cat _build/default/copied/file
  contents
  $ dune_cmd stat kind _build/default/copied/file
  regular file

An original input symlink is also copied into a regular file.

  $ dune build copied-link --sandbox symlink
  $ cat _build/default/copied-link/link
  contents
  $ dune_cmd stat kind _build/default/copied-link/link
  regular file

Copy and hardlink modes resolve the original input link during population.

  $ dune build copied-link --sandbox copy
  $ cat _build/default/copied-link/link
  contents
  $ dune_cmd stat kind _build/default/copied-link/link
  regular file

  $ dune build copied-link --sandbox hardlink
  $ cat _build/default/copied-link/link
  contents
  $ dune_cmd stat kind _build/default/copied-link/link
  regular file

The action-created link uses the target text of a symlink-sandbox input, but
lives at another path and points to the local file. It is resolved normally
in every mode, without interpreting the link text as a sandbox input.

  $ dune build copied-local --sandbox symlink
  $ cat _build/default/copied-local/alias
  local
  $ dune_cmd stat kind _build/default/copied-local/alias
  regular file

  $ dune build copied-local --sandbox copy
  $ cat _build/default/copied-local/alias
  local
  $ dune_cmd stat kind _build/default/copied-local/alias
  regular file

  $ dune build copied-local --sandbox hardlink
  $ cat _build/default/copied-local/alias
  local
  $ dune_cmd stat kind _build/default/copied-local/alias
  regular file

An action-created directory link points outside the copied tree. Its contents
are copied into a real directory.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir copied-directory))
  >  (deps (sandbox always))
  >  (action
  >   (no-infer
  >    (progn
  >     (system "mkdir -p input real && echo nested > real/file")
  >     (system "ln -s ../real input/alias")
  >     (copy input copied-directory)))))
  > EOF

  $ dune build copied-directory --sandbox symlink
  $ cat _build/default/copied-directory/alias/file
  nested
  $ dune_cmd stat kind _build/default/copied-directory/alias
  directory
  $ dune_cmd stat kind _build/default/copied-directory/alias/file
  regular file

A cyclic directory link still fails with the filesystem's symlink limit,
rather than hanging.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir copied-cycle))
  >  (deps (sandbox always))
  >  (action
  >   (no-infer
  >    (progn
  >     (system "mkdir input && ln -s . input/loop")
  >     (copy input copied-cycle)))))
  > EOF

  $ dune build copied-cycle --sandbox symlink > cycle.log 2>&1
  [1]
  $ grep -o 'Too many levels of symbolic links' cycle.log
  Too many levels of symbolic links
