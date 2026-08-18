Duplicate entries in solve_for_platforms currently break result merging. Record the
failure before joint solving deduplicates the requested platform set.

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

Merging successful per-platform results rejects the duplicate solver
environment:

  $ dune pkg lock >output 2>&1
  [1]
  $ grep 'Tried to add duplicate solver env' output
    ("Tried to add duplicate solver env to lockdir conditional choice",

When solving fails before result merging, the duplicate platform is reported
twice:

  $ mkpkg foo <<'EOF'
  > available: false
  > EOF
  $ dune pkg lock >output 2>&1
  [1]
  $ grep 'arch = arm64; os = macos' output
  - arch = arm64; os = macos
  - arch = arm64; os = macos
  $ grep 'arch = x86_64; os = linux' output
  - arch = x86_64; os = linux
