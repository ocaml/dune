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

Symlink mode currently rejects the sandbox link to the same regular input.

  $ dune build copied --sandbox symlink
  File "dune", lines 9-12, characters 0-104:
   9 | (rule
  10 |  (target (dir copied))
  11 |  (deps tree/file (sandbox always))
  12 |  (action (no-infer (copy tree copied))))
  Error: Symlink file is not allowed here
  [1]

Symlink mode rejects an original input symlink inside the copied directory.

  $ dune build copied-link --sandbox symlink
  File "dune", lines 13-16, characters 0-114:
  13 | (rule
  14 |  (target (dir copied-link))
  15 |  (deps tree/link (sandbox always))
  16 |  (action (no-infer (copy tree copied-link))))
  Error: Symlink link is not allowed here
  [1]

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
lives at another path and points to the local file. It is rejected in every mode.

  $ dune build copied-local --sandbox symlink
  File "dune", lines 17-26, characters 0-301:
  17 | (rule
  18 |  (target (dir copied-local))
  19 |  (deps tree/file (sandbox always))
  20 |  (action
  21 |   (no-infer
  22 |    (progn
  23 |     (system "mkdir -p a/b/c/tree default/tree")
  24 |     (system "echo local > default/tree/file")
  25 |     (system "ln -s ../../../../default/tree/file a/b/c/tree/alias")
  26 |     (copy a/b/c/tree copied-local)))))
  Error: Symlink alias is not allowed here
  [1]

  $ dune build copied-local --sandbox copy
  File "dune", lines 17-26, characters 0-301:
  17 | (rule
  18 |  (target (dir copied-local))
  19 |  (deps tree/file (sandbox always))
  20 |  (action
  21 |   (no-infer
  22 |    (progn
  23 |     (system "mkdir -p a/b/c/tree default/tree")
  24 |     (system "echo local > default/tree/file")
  25 |     (system "ln -s ../../../../default/tree/file a/b/c/tree/alias")
  26 |     (copy a/b/c/tree copied-local)))))
  Error: Symlink alias is not allowed here
  [1]

  $ dune build copied-local --sandbox hardlink
  File "dune", lines 17-26, characters 0-301:
  17 | (rule
  18 |  (target (dir copied-local))
  19 |  (deps tree/file (sandbox always))
  20 |  (action
  21 |   (no-infer
  22 |    (progn
  23 |     (system "mkdir -p a/b/c/tree default/tree")
  24 |     (system "echo local > default/tree/file")
  25 |     (system "ln -s ../../../../default/tree/file a/b/c/tree/alias")
  26 |     (copy a/b/c/tree copied-local)))))
  Error: Symlink alias is not allowed here
  [1]

An action-created directory link points outside the copied tree. This should
copy the directory contents, but currently rejects the link.

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

  $ dune build copied-directory --sandbox symlink > directory.log 2>&1
  [1]
  $ grep '^Error:' directory.log
  Error: Symlink alias is not allowed here

A cyclic directory link must fail rather than hang.

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
  $ grep '^Error:' cycle.log
  Error: Symlink loop is not allowed here
