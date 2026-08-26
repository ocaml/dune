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

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = x86_64; os = linux:
  - holder.0.0.1
  
  arch = x86_64; os = macos:
  - needs-target.0.0.1
  - target.1
  $ test -e dune.lock/holder.0.0.1.pkg
  $ test -e dune.lock/target.1.pkg

An incompatible transitive dependency must still report the actual version
restriction rather than attributing the failure to the conflict class.

  $ mkpkg needs-impossible <<'EOF'
  > available: os = "macos"
  > depends: [ "target" {>= "2"} ]
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "holder" {os = "linux"}
  >   "needs-impossible" {os = "macos"}
  > ]
  > EOF
  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = x86_64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: holder.0.0.1 needs-impossible.0.0.1 x.dev
  - target -> (problem) on arch = x86_64; os = macos
      needs-impossible 0.0.1 requires >= 2
      Rejected candidates:
        target.1: Incompatible with restriction: >= 2
  [1]
