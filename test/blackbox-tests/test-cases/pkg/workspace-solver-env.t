Workspace solver overrides include workspace dev dependencies without enabling
pp's dev-only OCamlFormat pin. A pinned package remains a dependency even when
its sources are local and contain a native Dune project.

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled
  $ mkrepo
  $ mkpkg dev-tool
  $ mkpkg test-tool

  $ mkdir _pp _ocamlformat
  $ cat >_pp/dune-project <<'EOF'
  > (lang dune 3.26)
  > EOF
  $ cat >_pp/pp.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "ocamlformat" { with-dev-setup & = "0.15.0" }
  >   "pp-tests" { with-test }
  > ]
  > pin-depends: [ "ocamlformat.0.15.0" "file://$PWD/_ocamlformat" ]
  > build: [
  >   [ "echo" "ordinary" ]
  >   [ "echo" "pp-dev" ] { with-dev-setup }
  > ]
  > EOF
  $ cat >_ocamlformat/ocamlformat.opam <<'EOF'
  > opam-version: "2.0"
  > EOF

The dev dependency is on a second workspace package, reached from app.

  $ write_project() {
  > cat >dune-project <<EOF
  > (lang dune 3.26)
  > (pin
  >  (url "file://$PWD/_pp")
  >  (package (name pp)))
  > (package (name app) (depends bar))
  > (package (name bar) (depends pp $1) $2)
  > EOF
  > }
  $ write_project '(dev-tool :with-dev-setup)' ''

  $ write_workspace() {
  > cat >dune-workspace <<EOF
  > (lang dune $1)
  > (pkg enabled)
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solver_env $2)
  >  $3)
  > EOF
  > }

Shared settings enable pp's dev dependencies as well.

  $ write_workspace 3.26 '(with-dev-setup true)' ''
  $ dune pkg lock
  Solution for dune.lock:
  - dev-tool.0.0.1
  - ocamlformat.0.15.0
  - pp.dev
  $ head -n1 dune.lock/lock.dune
  (lang package 0.1)

Old flat solver metadata is rejected with a re-locking hint.

  $ cat >dune.lock/lock.dune <<'EOF'
  > (lang package 0.1)
  > (repositories (complete false) (used))
  > (expanded_solver_variable_bindings
  >  (variable_values (with-dev-setup true)))
  > EOF
  $ dune pkg validate-lockdir
  Failed to parse lockdir dune.lock:
  File "dune.lock/lock.dune", line 4, characters 1-40:
  Error: This lock directory uses obsolete flat solver-variable bindings.
  Regenerate it with separate workspace and dependency bindings.
  Hint: 'dune pkg lock'
  
  Error: Some lockdirs do not contain solutions for local packages:
  - dune.lock
  [1]

Re-locking includes the workspace dev tool but excludes OCamlFormat.

  $ write_workspace 3.26 '' '(workspace_solver_env (with-dev-setup true))'
  $ dune pkg lock
  Solution for dune.lock:
  - dev-tool.0.0.1
  - pp.dev
  $ head -n1 dune.lock/lock.dune
  (lang package 0.1)
  $ test ! -f dune.lock/ocamlformat.pkg
  $ grep -o '(run echo [^)]*)' dune.lock/pp.pkg
  (run echo ordinary)
  $ grep -A20 expanded_solver_variable_bindings dune.lock/lock.dune
  (expanded_solver_variable_bindings
   (workspace
    (variable_values
     (with-dev-setup true)))
   (dependencies
    (variable_values
     (with-test false)
     (with-dev-setup false))))

The recorded workspace environment is used for dependency reconstruction.

  $ dune pkg validate-lockdir
  $ dune describe pkg list-locked-dependencies --transitive
  Dependencies of local packages locked in dune.lock
  - Transitive dependencies of local package app.dev
    - bar.dev
    - dev-tool.0.0.1
    - pp.dev
    
  - Transitive dependencies of local package bar.dev
    - dev-tool.0.0.1
    - pp.dev
    

Changing a consulted workspace variable requires re-locking.

  $ write_workspace 3.26 '' '(workspace_solver_env (with-dev-setup false))'
  $ dune pkg validate-lockdir
  Error: The solver environment for workspace packages has changed: variable
  "with-dev-setup" was true and is now false. Regenerate the lock directory.
  Hint: 'dune pkg lock'
  [1]

A workspace override does not alter the shared dependency environment.

  $ write_workspace 3.26 '(with-dev-setup true)' '(workspace_solver_env (with-dev-setup false))'
  $ dune pkg lock
  Solution for dune.lock:
  - ocamlformat.0.15.0
  - pp.dev
  $ test ! -f dune.lock/dev-tool.pkg
  $ test -f dune.lock/ocamlformat.pkg

Automatic locks are invalidated by workspace overrides too.

  $ rm -r dune.lock
  $ write_workspace 3.26 '' '(workspace_solver_env (with-dev-setup false))'
  $ dune build _build/_private/default/.lock/dune.lock
  $ test ! -f _build/_private/default/.lock/dune.lock/dev-tool.pkg
  $ write_workspace 3.26 '' '(workspace_solver_env (with-dev-setup true))'
  $ dune build _build/_private/default/.lock/dune.lock
  $ test -f _build/_private/default/.lock/dune.lock/dev-tool.pkg
  $ test ! -f _build/_private/default/.lock/dune.lock/ocamlformat.pkg

An explicit workspace assignment equal to an inherited value reuses the solve.

  $ write_workspace 3.26 '' '(workspace_solver_env (with-dev-setup true) (with-doc false))'
  $ dune build _build/_private/default/.lock/dune.lock
  $ dune trace cat | jq -s 'include "dune"; any(.[] | actionNames; . == "lock")'
  false
  $ build_pkg dev-tool

Workspace test overrides can enable tests despite a shared false setting.

  $ write_project '(dev-tool :with-dev-setup) (test-tool :with-test)' ''
  $ write_workspace 3.26 '(with-test false)' '(workspace_solver_env (with-dev-setup true) (with-test true))'
  $ dune pkg lock
  Solution for dune.lock:
  - dev-tool.0.0.1
  - pp.dev
  - test-tool.0.0.1
  $ dune pkg validate-lockdir
  $ dune describe pkg list-locked-dependencies --transitive
  Dependencies of local packages locked in dune.lock
  - Transitive dependencies of local package app.dev
    - bar.dev
    - dev-tool.0.0.1
    - pp.dev
    
  - Transitive dependencies of local package bar.dev
    - dev-tool.0.0.1
    - pp.dev
    - test-tool.0.0.1 (test only)

Platform identity cannot be overridden for just the workspace.

  $ write_workspace 3.26 '' '(workspace_solver_env (os other))'
  $ dune pkg print-solver-env
  File "dune-workspace", line 9, characters 23-33:
  9 |  (workspace_solver_env (os other)))
                             ^^^^^^^^^^
  Error: Platform variable "os" cannot be overridden in workspace_solver_env.
  [1]

The field is version-gated.

  $ write_workspace 3.25 '' '(workspace_solver_env (with-dev-setup true))'
  $ dune pkg print-solver-env
  File "dune-workspace", line 9, characters 1-45:
  9 |  (workspace_solver_env (with-dev-setup true)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'workspace_solver_env' is only available since version 3.26 of the
  dune language. Please update your dune-project file to have (lang dune 3.26).
  [1]

The distinction applies to arbitrary variables, availability, conflicts, and
filtered actions, not only the dev-setup flag. Shared unsets are applied before
workspace assignments.

  $ cat >_pp/pp.opam <<EOF
  > opam-version: "2.0"
  > available: scope-flag = "dependency"
  > conflicts: [ "dev-tool" { scope-flag = "workspace" } ]
  > depends: [
  >   "ocamlformat" { with-dev-setup & = "0.15.0" }
  >   "pp-tests" { with-test }
  >   "pp-docs" { ? with-doc }
  > ]
  > pin-depends: [ "ocamlformat.0.15.0" "file://$PWD/_ocamlformat" ]
  > build: [
  >   [ "echo" "ordinary" ]
  >   [ "echo" "dependency-env" ] { scope-flag = "dependency" }
  >   [ "echo" "pp-dev" ] { with-dev-setup }
  > ]
  > EOF
  $ write_project '(dev-tool (and (= :scope-flag workspace) (= :with-doc false)))' '(conflicts (pp (= :scope-flag dependency)))'
  $ write_workspace 3.26 '(scope-flag dependency)' '(workspace_solver_env (scope-flag workspace) (with-doc false)) (unset_solver_vars with-doc)'
  $ dune pkg lock
  Solution for dune.lock:
  - dev-tool.0.0.1
  - pp.dev
  $ grep -o '(run echo [^)]*)' dune.lock/pp.pkg
  (run echo ordinary)
  (run echo dependency-env)
  $ grep -A20 expanded_solver_variable_bindings dune.lock/lock.dune
  (expanded_solver_variable_bindings
   (workspace
    (variable_values
     (with-doc false)
     (scope-flag workspace)))
   (dependencies
    (variable_values
     (with-test false)
     (with-dev-setup false)
     (scope-flag dependency))
    (unset_variables with-doc)))
  $ dune pkg validate-lockdir
  $ build_pkg dev-tool

Only consulted variables participate in subsequent lock validation.

  $ write_workspace 3.26 '(scope-flag dependency)' '(workspace_solver_env (scope-flag workspace) (with-doc false) (unused arbitrary)) (unset_solver_vars with-doc)'
  $ dune pkg validate-lockdir
  $ dune pkg print-solver-env 2>&1 | grep -E '^(Dependencies|Workspace)|scope-flag|with-doc'
  Dependencies:
  - scope-flag = dependency
  Workspace:
  - scope-flag = workspace
  - with-doc = false

Shared unset variables are also validated in the dependency scope.

  $ write_workspace 3.26 '(scope-flag dependency) (with-doc false)' '(workspace_solver_env (scope-flag workspace) (with-doc false))'
  $ dune pkg validate-lockdir
  Error: The solver environment for dependency packages has changed: variable
  "with-doc" was unset and is now false. Regenerate the lock directory.
  Hint: 'dune pkg lock'
  [1]

Build-time validation also checks the recorded workspace scope.

  $ write_workspace 3.26 '(scope-flag dependency)' '(workspace_solver_env (scope-flag changed) (with-doc false)) (unset_solver_vars with-doc)'
  $ build_pkg dev-tool
  Error: The solver environment for workspace packages has changed: variable
  "scope-flag" was workspace and is now changed. Regenerate the lock directory.
  Hint: 'dune pkg lock'
  [1]

Portable lock directories combine the recorded workspace bindings with the
shared platform identity when reconstructing local dependencies.

  $ write_project '(dev-tool (and (= :scope-flag workspace) (= :with-doc false) (= :os linux)))' '(conflicts (pp (= :scope-flag dependency)))'
  $ write_workspace 3.26 '(scope-flag dependency)' '(workspace_solver_env (scope-flag workspace) (with-doc false)) (unset_solver_vars with-doc)'
  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - pp.dev
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - dev-tool.0.0.1
  
  arch = x86_64; os = linux:
  - dev-tool.0.0.1
  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg validate-lockdir
  $ grep -o '(run echo [^)]*)' dune.lock/pp.dev.pkg
  (run echo ordinary)
  (run echo dependency-env)

Even an empty explicit scope requires scoped metadata, including when no
variables were consulted.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.26)
  > (package (name app))
  > EOF
  $ write_workspace 3.26 '' '(workspace_solver_env)'
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
  $ head -n1 dune.lock/lock.dune
  (lang package 0.1)
  $ dune pkg validate-lockdir

The defaults are shared assignments plus the workspace test override.

  $ dune pkg print-solver-env 2>&1 | grep -E '^(Dependencies|Workspace)|with-(test|doc|dev-setup)'
  Dependencies:
  - with-dev-setup = false
  - with-doc = false
  - with-test = false
  Workspace:
  - with-dev-setup = false
  - with-doc = false
  - with-test = true

Removing an empty override does not change the resolved environments.

  $ write_workspace 3.26 '' ''
  $ dune pkg validate-lockdir
