Regression test: when a package installs a built directory target via
`(install (dirs ...))`, a consumer rule with `(deps (package ...))` must
not crash. The layout has to symlink the directory entry with
`Action_builder.symlink_dir`, not plain `symlink`.

  $ make_dune_project_with_package 3.24 dirpkg

  $ mkdir src
  $ cat >src/dune <<EOF
  > (library (public_name dirpkg))
  > (rule
  >  (target (dir generated))
  >  (action
  >   (progn (system "mkdir %{target}")
  >          (system "echo top > %{target}/a.txt")
  >          (system "mkdir %{target}/sub")
  >          (system "echo nested > %{target}/sub/b.txt"))))
  > (install (section share) (package dirpkg) (dirs generated))
  > EOF
  $ cat >src/dirpkg.ml <<'EOF'
  > let x = 1
  > EOF

A rule that depends on the package must build without crashing. Without
the kind-aware dispatch, the layout's `share/dirpkg/generated` symlink
would be created as a file pointing at a directory; the action then asks
for `generated` as a file and the engine reports "No rule found".

  $ cat >dune <<EOF
  > (rule
  >  (deps (package dirpkg))
  >  (action (with-stdout-to out (echo ok))))
  > EOF

  $ dune build out
  $ cat _build/default/out
  ok

Spot-check that the installed directory is reachable as a path via
`%{pkg:...}`:

  $ cat >dune <<EOF
  > (rule
  >  (alias check-dir)
  >  (deps (package dirpkg))
  >  (action (bash "cat %{pkg:dirpkg:share:generated}/a.txt")))
  > EOF

  $ dune build @check-dir
  top
