We try to pull an opam package that isn't a dune project

  $ mkrepo
  $ add_mock_repo_if_needed

  $ make_project_pinned_to_foo

  $ mkdir _foo
  $ cat >_foo/foo.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "foo" ]
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.dev
  $ pkg="${default_lock_dir}/foo.dev.pkg"
  $ grep version $pkg
  (version dev)
  $ grep dev $pkg
  (version dev)
  (dev)
  $ grep "file://" $pkg | dune_cmd subst "$PWD" PWD
     file://PWD/_foo)))
