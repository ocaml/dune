Separate platform solves minimize avoid-version packages independently. They
therefore select two distinct platform-specific avoid-version packages instead
of the single common alternative.

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

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = x86_64; os = linux:
  - linux-alt.0.0.1 (this version should be avoided)
  
  arch = x86_64; os = macos:
  - macos-alt.0.0.1 (this version should be avoided)
