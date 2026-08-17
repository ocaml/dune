Existing language versions permit a portable lock directory to select different
versions on different platforms.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg foo 1 <<'EOF'
  > available: os = "linux"
  > EOF
  $ mkpkg foo 2 <<'EOF'
  > available: os = "macos"
  > EOF
  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=x86_64 dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  (none)
  
  Additionally, some packages will only be built on specific platforms.
  
  arch = arm64; os = linux:
  - foo.1
  
  arch = arm64; os = macos:
  - foo.2
  
  arch = x86_64; os = linux:
  - foo.1
  
  arch = x86_64; os = macos:
  - foo.2
