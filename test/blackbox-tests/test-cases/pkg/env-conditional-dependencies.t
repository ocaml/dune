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

Create a workspace config that defines separate build contexts for macos and linux.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (lock_dir
  >  (path dune.no-os.lock)
  >  (repositories mock)
  >  (solver_env
  >   (unset_variables os)))
  > (lock_dir
  >  (path dune.linux.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os linux)))
  > (lock_dir
  >  (path dune.macos.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os macos)))
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (context
  >  (default
  >   (name linux)
  >   (lock_dir dune.linux.lock)))
  > (context
  >  (default
  >   (name macos)
  >   (lock_dir dune.macos.lock)))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Now the os-specific dependencies are included on their respective systems.
  $ dune pkg lock --all
  Solution for dune.linux.lock:
  (no dependencies to lock)
  Solution for dune.lock:
  (no dependencies to lock)
  Solution for dune.macos.lock:
  (no dependencies to lock)
  Solution for dune.no-os.lock:
  (no dependencies to lock)
