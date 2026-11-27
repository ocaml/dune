Test which "with-doc" and "with-dev-setup" dependencies are locked. Starting
with Dune 3.26, "with-dev-setup" applies to local packages by default, like
"with-test". The workspace selects which packages each of them applies to.

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

  $ write_project() {
  > cat > dune-project <<EOF
  > (lang dune $1)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends
  >   dep
  >   (doc-dep :with-doc)
  >   (dev-setup-dep :with-dev-setup)))
  > EOF
  > }

Before Dune 3.26, doc and dev-setup dependencies are not locked:

  $ write_project 3.25
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1

Starting with Dune 3.26, dev-setup dependencies of local packages are locked,
but not those of locked packages:

  $ write_project 3.26
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1
  - dev-setup-dep.0.0.1

The doc and dev-setup build steps of locked packages are disabled:

  $ grep -A 3 '^(build' ${default_lock_dir}/dep.*.pkg
  (build
   (all_platforms ((action (run echo build)))))

  $ dune pkg validate-lockdir

The workspace can disable them:

  $ write_workspace() {
  > cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (lock_dir
  >  (repositories mock)
  >  $1)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF
  > }

  $ write_workspace "(qualifiers with-test)"
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1

  $ dune pkg validate-lockdir

Or enable them for local packages, which is the default for "with-dev-setup":

  $ write_workspace "(qualifiers with-test with-doc with-dev-setup)"
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1
  - dev-setup-dep.0.0.1
  - doc-dep.0.0.1

  $ grep -A 3 '^(build' ${default_lock_dir}/dep.*.pkg
  (build
   (all_platforms ((action (run echo build)))))

Setting them in "solver_env" is still supported: "true" applies them to all
packages and "false" to none:

  $ write_workspace "(solver_env (with-doc true) (with-dev-setup false))"
  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep.0.0.1
  - doc-dep.0.0.1
  - transitive-doc-dep.0.0.1

They cannot be set both ways:

  $ write_workspace "(qualifiers with-doc) (solver_env (with-doc true))"
  $ dune pkg lock
  File "dune-workspace", line 4, characters 35-50:
  4 |  (qualifiers with-doc) (solver_env (with-doc true)))
                                         ^^^^^^^^^^^^^^^
  Error: "with-doc" is set both in 'solver_env' and in 'qualifiers', which is
  not allowed.
  [1]

The field is only available starting with Dune 3.26:

  $ write_workspace "(qualifiers with-doc)"
  $ sed -i.bak 's/(lang dune 3.26)/(lang dune 3.25)/' dune-workspace
  $ dune pkg lock
  File "dune-workspace", line 4, characters 1-22:
  4 |  (qualifiers with-doc))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: 'qualifiers' is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  [1]
