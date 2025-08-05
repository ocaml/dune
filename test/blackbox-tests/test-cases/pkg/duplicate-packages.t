A workspace with a package that exists in the lock file and in the workspace
shouldn't be allowed (for now)

  $ . ./helpers.sh

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ cat > mypkg.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ make_lockdir
  $ touch ${default_lock_dir}/mypkg.lock

  $ dune build

It should also fail when we define the package only in the dune-project file:

  $ rm mypkg.opam
  $ cat >>dune-project <<EOF
  > (package
  >  (allow_empty)
  >  (name mypkg))
  > EOF

  $ dune build
