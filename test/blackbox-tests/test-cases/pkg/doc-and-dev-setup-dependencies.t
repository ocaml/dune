Test which "with-doc" and "with-dev-setup" dependencies are locked. Like
"with-test", these variables only apply to local packages.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg doc-dep
  $ mkpkg dev-setup-dep
  $ mkpkg transitive-doc-dep
  $ mkpkg transitive-dev-setup-dep
  $ mkpkg dep <<EOF
  > depends: [
  >   "transitive-doc-dep" {with-doc}
  >   "transitive-dev-setup-dep" {with-dev-setup}
  > ]
  > build: [
  >   ["echo" "build"]
  >   ["echo" "doc"] {with-doc}
  >   ["echo" "dev-setup"] {with-dev-setup}
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends
  >   dep
  >   (doc-dep :with-doc)
  >   (dev-setup-dep :with-dev-setup)))
  > EOF

Doc and dev-setup dependencies of local packages are locked, but not those of
locked packages:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1
  - dev-setup-dep.0.0.1
  - doc-dep.0.0.1

The doc and dev-setup build steps of locked packages are disabled:

  $ grep -A 3 '^(build' ${default_lock_dir}/dep.*.pkg
  (build
   (all_platforms ((action (run echo build)))))

  $ dune pkg validate-lockdir

The workspace can disable doc and dev-setup dependencies of local packages:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (with-doc false)
  >   (with-dev-setup false)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1

  $ dune pkg validate-lockdir
