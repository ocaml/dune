A local package dependency alternative may select a different package on each
platform. Preserve every package selected by the per-platform solves when
constructing the portable lock directory.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg linux-impl <<'EOF'
  > available: os = "linux"
  > EOF
  $ mkpkg macos-impl <<'EOF'
  > available: os = "macos"
  > EOF

The local package accepts either implementation. Package availability forces
Linux and macOS to select different branches of the disjunction.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "linux-impl" | "macos-impl" ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - linux-impl.0.0.1
  
  arch = arm64; os = macos:
  - macos-impl.0.0.1
  
  arch = x86_64; os = linux:
  - linux-impl.0.0.1
  
  arch = x86_64; os = macos:
  - macos-impl.0.0.1
  $ ls dune.lock/*.pkg | sort
  dune.lock/linux-impl.0.0.1.pkg
  dune.lock/macos-impl.0.0.1.pkg
