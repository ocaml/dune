Test that we can read package metadata from opam files.
  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg a 0.1
  $ mkpkg a 0.6

  $ mkpkg b 0.1
  $ mkpkg b 0.6

  $ mkpkg c 0.1

  $ mkpkg d

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > EOF

  $ cat > foo.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "a" { < "0.5" }
  >   "b" { "bar" = "bar" }
  >   "c" { < "0.2" & with-doc }
  > ]
  > EOF

  $ cat > bar.opam <<EOF
  > opam-version: "2.0"
  > depends: [
  >   "d"
  >   "foo"
  > ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - a.0.1
  - b.0.6
  - d.0.0.1

  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package bar.dev
    - d.0.0.1
    - foo.dev
    
  - Immediate dependencies of local package foo.dev
    - a.0.1
    - b.0.6
    

  $ cat > dune-workspace <<EOF
  > (lang dune 3.12)
  > (lock_dir
  >  (repositories mock)
  >  (solver_env
  >   (with-doc true)))
  > (repository
  >  (name mock)
  >  (source "$PWD/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - a.0.1
  - b.0.6
  - c.0.1
  - d.0.0.1

  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package bar.dev
    - d.0.0.1
    - foo.dev
    
  - Immediate dependencies of local package foo.dev
    - a.0.1
    - b.0.6
    - c.0.1
    
