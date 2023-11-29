Having an invalid package dependency should give a good user message rather than raising
an uncaught exception. It is very likely that users will type foo.1.2.3 for a package
version due to the convention in opam. In this case we could also give a hint how to write
it in a dune-project file.

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name invalid)
  >  (depends foo.1.2.3))
  > EOF

  $ dune pkg lock 2>&1 | head -n1
  Error: exception Failure("Invalid character in package name \"foo.1.2.3\"")
