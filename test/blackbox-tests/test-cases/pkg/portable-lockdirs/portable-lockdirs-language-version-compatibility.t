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
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: foo.1 x.dev
  - foo -> (problem) on arch = arm64; os = macos
      Rejected candidates:
        foo.2:
          Reason for rejection unknown:
          x.dev=true && foo.2=false => (no solution found)=true
        foo.1: Availability condition not satisfied
  - foo -> (problem) on arch = x86_64; os = macos
      Rejected candidates:
        foo.2:
          Reason for rejection unknown:
          x.dev=true && foo.2=false => (no solution found)=true
        foo.1: Availability condition not satisfied
  [1]
