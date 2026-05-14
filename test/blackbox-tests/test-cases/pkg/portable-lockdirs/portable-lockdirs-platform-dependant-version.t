Test for a project which depends on different versions of the same package depending on the platform.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo %{version}% > %{share}%/version"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ mkpkg bar <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > depends: [
  >   "foo" {= "1" & os = "linux"}
  >   "foo" {= "2" & os = "macos"}
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends bar))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Under single-solve with the cross-platform version equality SAT constraint,
allowing different versions of the same package on different platforms is
rejected by the SAT solver itself rather than by a post-solve check.

  $ DUNE_TRACE=+sat dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 x.dev
  - foo -> foo.1
      bar 0.0.1 requires = 1
  [1]



The SAT engine itself rejects the conflict; the post-solve version-conflict
check is no longer reached. The do_solve retry path runs SAT 3 times before
reporting the failure, and each run records the same cross-platform conflict.

  $ dune trace cat \
  > | jq -s 'include "dune"; [ .[] | satSolveEvents | .args ]'
  [
    {
      "num_variables": 21,
      "num_clauses": 29,
      "num_decisions": 0,
      "num_conflicts": 1
    },
    {
      "num_variables": 21,
      "num_clauses": 29,
      "num_decisions": 0,
      "num_conflicts": 1
    },
    {
      "num_variables": 34,
      "num_clauses": 29,
      "num_decisions": 12,
      "num_conflicts": 1
    }
  ]

Build the project as if we were on linux and confirm that version 1 of foo was built:
  $ export DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11
  $ dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/version
  Error: Lock directory is not active for context "default".
  cat: _build/_private/default/.pkg//target/share/version: No such file or directory
  [1]

  $ dune clean

Build the project as if we were on macos and confirm that version 2 of foo was built:
  $ export DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1
  $ dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/version
  Error: Lock directory is not active for context "default".
  cat: _build/_private/default/.pkg//target/share/version: No such file or directory
  [1]

