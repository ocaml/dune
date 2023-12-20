Having an invalid package dependency that looks like an opam-versioned package
gives a good user message rather. It is very likely that users will type
foo.1.2.3 for a package version due to the convention in opam.
In this case we could also hint at the correct syntax for dune-project files.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.2.3
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name invalid)
  >  (depends foo.1.2.3))
  > EOF
  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock 2>&1
  File "dune-project", line 4, characters 10-19:
  4 |  (depends foo.1.2.3))
                ^^^^^^^^^
  Error: "foo.1.2.3" is an invalid package dependency.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: (foo (= 1.2.3)) would be a correct package dependency
  [1]
