When Dune generates a lockfile for a package, if the package had the default
Dune build command, use the "dune" keyword in the lockfile rather than copying
its build command.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline Foo.foo
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF

Lock, build, and run the executable in the project:
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.0.0.1

The lockfile contains the "dune" keyword rather than the build command:
  $ cat dune.lock/foo.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((dune))))
