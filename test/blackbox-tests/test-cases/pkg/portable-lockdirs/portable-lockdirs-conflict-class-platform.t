Conflict classes are platform-local. Selecting a class holder on Linux must not
make a package on macOS appear unavailable because it has the same conflict
class.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg holder <<'EOF'
  > available: os = "linux"
  > conflict-class: "shared"
  > EOF
  $ mkpkg target 1 <<'EOF'
  > available: os = "macos"
  > conflict-class: "shared"
  > EOF
  $ mkpkg needs-target <<'EOF'
  > available: os = "macos"
  > depends: [ "target" ]
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "holder" {os = "linux"}
  >   "needs-target" {os = "macos"}
  > ]
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64) (os linux))
  >   ((arch x86_64) (os macos))))
  > (pkg enabled)
  > EOF

The transitive macOS dependency and the Linux package must coexist even though
they belong to the same conflict class on different platforms.

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock >/dev/null 2>&1
  $ test -e dune.lock/holder.0.0.1.pkg
  $ test -e dune.lock/target.1.pkg
