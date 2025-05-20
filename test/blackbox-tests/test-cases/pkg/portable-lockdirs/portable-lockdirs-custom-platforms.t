Specifying custom platforms to solve for.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Create a package that writes a different value to some files depending on the os, with a non-defaut os.
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo OpenBSD > %{share}%/kernel"] { os = "openbsd" }
  >   ["sh" "-c" "echo Other > %{share}%/kernel"] { os != "openbsd" }
  > ]
  > EOF

Create a custom dune-workspace to solve for openbsd.
  $ cat > dune-workspace <<EOF
  > (lang dune 3.18)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os openbsd))))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1

  $ cat dune.lock/lock.dune
  (lang package 0.1)
  
  (dependency_hash 36e640fbcda71963e7e2f689f6c96c3e)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os openbsd)))

Build as though we were on openbsd.
  $ DUNE_CONFIG__OS=openbsd DUNE_CONFIG__ARCH=x86_64 dune build
  $ cat $pkg_root/foo/target/share/kernel
  OpenBSD

Now building on linux won't work:
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  File "dune.lock/lock.dune", lines 10-11, characters 1-31:
  10 |  ((arch x86_64)
  11 |   (os openbsd)))
  Error: The lockdir does not contain a solution compatible with the current
  platform.
  The current platform is:
  - arch = x86_64
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  Hint: Try adding the following to dune-workspace:
  Hint: (lock_dir (solve_for_platforms ((arch x86_64) (os linux))))
  Hint: ...and then rerun 'dune pkg lock'
  [1]
  $ cat $pkg_root/foo/target/share/kernel
  OpenBSD

Update dune-workspace again, this time listing no platforms to demonstrate the
error case.
  $ cat > dune-workspace <<EOF
  > (lang dune 3.18)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms))
  > EOF

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  File "dune-workspace", line 7, characters 1-22:
  7 |  (solve_for_platforms))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: No platforms were specified for solving dependencies.
  Hint: Specify at least one platform here, or remove this field to solve for
  the default platforms.
  [1]
