Directory dependencies should expose nested files and empty directories.
Overlapping inputs must not hide a similarly prefixed sibling. Cover symlink
and copy sandboxes, which support directory inputs on 3.25.

  $ make_dune_project 3.25
  $ echo sibling > outer-other
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir outer))
  >  (action
  >   (progn
  >    (run mkdir -p outer/inner/empty)
  >    (system "echo nested > outer/inner/file"))))
  > (rule
  >  (alias directory)
  >  (deps outer outer-other (sandbox always))
  >  (action
  >   (progn
  >    (run cat outer/inner/file outer-other)
  >    (run test -d outer/inner/empty))))
  > (rule
  >  (alias overlap)
  >  (deps outer/inner/file outer-other outer/inner outer (sandbox always))
  >  (action
  >   (chdir outer/inner
  >    (progn
  >     (run cat file ../../outer-other)
  >     (run test -d empty)))))
  > EOF

  $ dune build @directory --sandbox symlink
  nested
  sibling
CR: Precreating a descendant's parent blocks the directory symlink.

  $ dune build @overlap --sandbox symlink 2>&1 | censor
  Error:
  symlink(_build/.sandbox/$DIGEST/default/outer): File exists
  -> required by alias overlap in dune:14
  [1]

  $ dune build @directory --sandbox copy
  nested
  sibling
  $ dune build @overlap --sandbox copy
  nested
  sibling
