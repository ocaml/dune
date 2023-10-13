  $ . ./helpers.sh
  $ mkrepo
 
A package with different linux and macos dependencies including a test-only
dependency:
  $ mkpkg foo <<EOF
  > depends: [
  >   "foo-linux" {os = "linux"}
  >   "foo-macos" {os = "macos"}
  >   "foo-macos-test-only" {os = "macos" & with-test}
  > ]
  > EOF
  $ mkpkg foo-linux <<EOF
  > available: os = "linux"
  > EOF
  $ mkpkg foo-linux-test-only <<EOF
  > available: os = "linux"
  > EOF
  $ mkpkg foo-macos <<EOF
  > available: os = "macos"
  > EOF

With no "os" variable set in the workspace, none of the os-specific
dependencies are included.
  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

Create a workspace config that defines separate build contexts for macos and linux.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (name linux)
  >   (lock dune.linux.lock)
  >   (solver_sys_vars
  >    (os linux))))
  > (context
  >  (default
  >   (name macos)
  >   (lock dune.macos.lock)
  >   (solver_sys_vars
  >    (os macos))))
  > EOF

Now the os-specific dependencies are included on their respective systems.
  $ dune pkg lock --dont-poll-system-solver-variables --opam-repository-path=mock-opam-repository --all-contexts
  Solution for dune.macos.lock:
  - foo.0.0.1
  - foo-macos.0.0.1
  Solution for dune.linux.lock:
  - foo.0.0.1
  - foo-linux.0.0.1
