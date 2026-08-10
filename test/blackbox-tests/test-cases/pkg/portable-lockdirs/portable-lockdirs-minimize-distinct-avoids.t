Joint solving minimizes distinct avoid-version package versions rather than
counting the same package once per selected platform.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg common <<'EOF'
  > flags: [avoid-version]
  > EOF
  $ mkpkg linux-alt <<'EOF'
  > available: os = "linux"
  > flags: [avoid-version]
  > EOF
  $ mkpkg macos-alt <<'EOF'
  > available: os = "macos"
  > flags: [avoid-version]
  > EOF
  $ mkpkg foo <<'EOF'
  > depends: [ "linux-alt" | "macos-alt" | "common" ]
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock)
  >  (solve_for_platforms
  >   ((arch x86_64)
  >    (os linux))
  >   ((arch x86_64)
  >    (os macos))))
  > (pkg enabled)
  > EOF
  $ write_portable_lockdirs_project

Selecting common on both platforms contributes one avoid-version package to
the lock directory. Selecting both platform-specific alternatives contributes
two.

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - common.0.0.1 (this version should be avoided)
  - foo.0.0.1
