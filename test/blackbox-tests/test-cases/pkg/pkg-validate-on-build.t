Test that we validate lockdirs before using them to build a package.

This test exercises Dune's ability to detect when it is asked to execute a
build plan on a platform for which the lock directory has no solution. The
analogous case for an explicitly configured platform set is tested in
"portable-lockdirs-custom-platforms".
  $ mkrepo

  $ mkpkg a <<EOF
  > EOF

  $ mkpkg b <<EOF
  > EOF

  $ mkpkg bar <<EOF
  > depends: [
  >   "a" { ? custom }
  >   "b" { os = "macos" }
  > ]
  > EOF

Helper function that creates a workspace file with a given solver env.
  $ generate_workspace() {
  >  cat <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories mock)
  > EOF
  >  cat
  >  cat <<EOF
  > )
  > (context
  >  (default
  >   (name default)))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF
  > }

  $ generate_workspace > dune-workspace <<EOF
  > (solver_env
  >  (os macos))
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - b.0.0.1
  - bar.0.0.1

When the workspace and lockdir is consistent we can build packages in the lockdir.
  $ build_pkg bar

Now change the workspace so that the "os" solver variable is changed, but don't
regenerate the lockdir, leaving the project in an inconsistent state.
  $ generate_workspace > dune-workspace <<EOF
  > (solver_env
  >  (os linux))
  > EOF

Print an error when attempting to build when the lockdir and workspace disagree
about the value of a variable.
  $ build_pkg bar
  File "dune.lock/lock.dune", lines 13-16, characters 1-58:
  13 |  ((arch x86_64)
  14 |   (os macos))
  15 |  ((arch arm64)
  16 |   (os macos)))
  Error: The lockdir does not contain a solution compatible with the current
  platform.
  The current platform is:
  - arch = x86_64
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  - sys-ocaml-version = 5.4.0+fake
  Hint: Try adding the following to dune-workspace:
  Hint: (lock_dir (solve_for_platforms ((arch x86_64) (os linux))))
  Hint: ...and then rerun 'dune pkg lock'
  [1]

Also detect the case where a variable that was unassigned at solve time and
used during solving is later given a value in the workspace file:
  $ generate_workspace > dune-workspace <<EOF
  > (solver_env
  >  (os macos)
  >  (custom foo))
  > EOF

  $ build_pkg bar
  Error: The dependency solution relies on the variable "custom" not being
  assigned a value but the solver environment in the workspace would assign it
  the value "foo".
  Hint: This can happen if the "solver_env" for the lockdir in the
  dune-workspace file has changed since generating the lockdir. Regenerate the
  lockdir by running:
  Hint: 'dune pkg lock'
  [1]

If we don't set a variable in the workspace but that variable appears in
lockdir metadata it's not an error because it might have got there by polling
the current system. Note that to prevent the "os" variable from being taken
from the current system it must be added to the unset variables in the
workspace.

  $ generate_workspace > dune-workspace <<EOF
  > (unset_solver_vars os)
  > EOF

Regenerating with [os] unset must not reintroduce it from the default platform
set:
  $ dune pkg lock >/dev/null
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  $ ! grep -q '(os ' dune.lock/lock.dune
  $ grep '(arch ' dune.lock/lock.dune | sort -u
  (solved_for_platforms ((arch x86_64)) ((arch arm64)))
