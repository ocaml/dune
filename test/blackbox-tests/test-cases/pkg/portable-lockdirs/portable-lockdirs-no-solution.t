Demonstrate the case where a project can't be solved at all.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make some packages that can't be coinstalled:
  $ mkpkg a <<EOF
  > depends: [
  >  "c" {= "0.1"}
  > ]
  > EOF

  $ mkpkg b <<EOF
  > depends: [
  >  "c" {= "0.2"}
  > ]
  > EOF

  $ mkpkg c "0.2"

Depend on a pair of packages which can't be coinstalled:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name foo)
  >  (depends a b))
  > EOF

Solver error when solving fails with the same error on all platforms:
  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: a.0.0.1 b.0.0.1 foo.dev
  - c -> (problem)
      a 0.0.1 requires = 0.1
      Rejected candidates:
        c.0.2: Incompatible with restriction: = 0.1
  [1]

Modify the "a" package so the solver error is different on different platforms:
  $ mkpkg a <<EOF
  > depends: [
  >  "c" {= "0.1" & os = "linux"}
  >  "c" {= "0.3" & os != "linux"}
  > ]
  > EOF

This time there will be two different solver errors. Both will be printed along
with the platforms where they are relevant:
  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: a.0.0.1 b.0.0.1 foo.dev
  - c -> (problem)
      a 0.0.1 requires = 0.1
      Rejected candidates:
        c.0.2: Incompatible with restriction: = 0.1
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: a.0.0.1 b.0.0.1 foo.dev
  - c -> (problem)
      a 0.0.1 requires = 0.3
      Rejected candidates:
        c.0.2: Incompatible with restriction: = 0.3
  [1]
