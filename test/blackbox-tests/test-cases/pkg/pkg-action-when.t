Testing the when action in lockfiles

  $ make_lockdir

Case with a mix of uncoditional and conditional actions in a progn action

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install
  >  (progn
  >   (when (= foo foo)
  >    (run echo a))
  >   (when (<> foo foo)
  >    (run echo b))
  >   (when (<> foo bar)
  >    (run echo c))
  >   (when (< 1 2)
  >    (run echo d))
  >   (when (< 2 1)
  >    (run echo e))))
  > EOF

  $ build_pkg test
  a
  c
  d

Version comparisons also work in lockfile actions and string expressions.

  $ make_lockpkg test <<'EOF'
  > (version 5.10)
  > (install
  >  (progn
  >   (when (ver> %{pkg-self:version} 5.9)
  >    (run echo numeric))
  >   (when (ver= 5.09 5.9)
  >    (run echo leading-zero))
  >   (when (ver< 5.9~beta1 5.9)
  >    (run echo prerelease))
  >   (when (and (ver>= 5.09 5.9) (not (ver<> 5.09 5.9)))
  >    (run echo equal))
  >   (run echo (ver<= 5.9 (concat 5 .10)))
  >   (run echo (ver= (when false 1.0) (when false 2.0)))))
  > EOF

  $ build_pkg test
  numeric
  leading-zero
  prerelease
  equal
  true
  true
