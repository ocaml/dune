Test that duplicate platform entries in solve_for_platforms are solved only
once, so the lock file does not contain duplicated platform conditions.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package:
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

  $ make_portable_lockdirs_project

Solve for a platform set that contains the same platform twice:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch arm64) (os macos))
  >   ((arch arm64) (os macos))
  >   ((arch x86_64) (os linux))))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

The solved_for_platforms metadata mentions each platform only once:
  $ grep -c 'os macos' ${default_lock_dir}/lock.dune
  1
  $ grep -c 'os linux' ${default_lock_dir}/lock.dune
  1
