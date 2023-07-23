Helper shell function that generates an opam file for a package:
  $ mkpkg() {
  >   name=$1
  >   mkdir -p mock-opam-repository/packages/$name/$name.0.0.1
  >   cat >mock-opam-repository/packages/$name/$name.0.0.1/opam
  > }

Helper shell function to generate a dune-project file and generate lockdir:
  $ solve_project() {
  >   cat >dune-project
  >   dune pkg lock --opam-repository-path=mock-opam-repository
  > }

Generate a mock opam repository including some test dependencies:
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

A package with different linux and macos dependencies including a test-only
dependency:
  $ mkpkg foo <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "foo-linux" {os = "linux"}
  >   "foo-macos" {os = "macos"}
  >   "foo-macos-test-only" {os = "macos" & with-test}
  > ]
  > EOF
  $ mkpkg foo-linux <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF
  $ mkpkg foo-linux-test-only <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > EOF
  $ mkpkg foo-macos <<EOF
  > opam-version: "2.0"
  > available: os = "macos"
  > EOF

Depending on foo should add the macos and linux dependency but not the
test-only dependency because we don't add transitive test dependencies:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF
  Solution for dune.lock:
  foo.0.0.1
  foo-linux.0.0.1
  foo-macos.0.0.1
  

A package with different linux and macos dependencies which are mutually
incompatible:
  $ mkpkg bar <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "bar-linux" {os = "linux"}
  >   "bar-macos" {os = "macos"}
  > ]
  > EOF
  $ mkpkg bar-linux <<EOF
  > opam-version: "2.0"
  > available: os = "linux"
  > conflicts: [
  >   "bar-macos"
  > ]
  > EOF
  $ mkpkg bar-macos <<EOF
  > opam-version: "2.0"
  > available: os = "macos"
  > conflicts: [
  >   "bar-linux"
  > ]
  > EOF

There's no solution to these dependencies:
  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (depends bar))
  > EOF
  Error: Unable to solve dependencies in build context: default
  Can't find all required versions.
  Selected: bar.0.0.1 bar-linux.0.0.1 x.dev
  - bar-macos -> (problem)
      bar-linux 0.0.1 requires conflict with all versions
      Rejected candidates:
        bar-macos.0.0.1: Incompatible with restriction: conflict with all
  versions
  [1]

Since there is no solution available with both the macos and linux dependencies
we need to generate a lockdir for a single os at a time. Create a workspace
config that defines separate build contexts for macos and linux.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name linux)
  >   (lock dune.linux.lock)
  >   (solver_env
  >    (sys
  >     (os linux)))))
  > (context
  >  (default
  >   (name macos)
  >   (lock dune.macos.lock)
  >   (solver_env
  >    (sys
  >     (os macos)))))
  > EOF

  $ dune pkg lock --opam-repository-path=mock-opam-repository --all-contexts
  Solution for dune.macos.lock:
  bar.0.0.1
  bar-macos.0.0.1
  
  Solution for dune.linux.lock:
  bar.0.0.1
  bar-linux.0.0.1
  
