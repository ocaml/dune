Make sure that vendored packages are kept by -p.

In the following test the package "bar" is vendored. We make sure that
"-p" never masks it.

  $ mkdir -p vendor/bar

  $ cat >dune-project <<EOF
  > (lang dune 2.1)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries bar))
  > (vendored_dirs vendor)
  > EOF

  $ cat >foo.ml<<EOF
  > print_endline Bar.message
  > EOF

  $ cat >vendor/bar/dune-project <<EOF
  > (lang dune 2.1)
  > (package (name bar))
  > EOF

  $ cat >vendor/bar/dune <<EOF
  > (library
  >  (public_name bar))
  > EOF

  $ cat >vendor/bar/bar.ml <<EOF
  > let message = "Hello from the bar!"
  > EOF

Without filtering anything:

  $ dune exec ./foo.exe
  Hello from the bar!

Keeping only "foo" via "-p" shouldn't mask "bar" since it is vendored:

  $ dune exec -p foo ./foo.exe
  Hello from the bar!

Asking to keep "bar" makes no sense since it is vendored:

  $ dune exec -p foo,bar ./foo.exe
  Error: Package bar is vendored and so will never be masked. It is redundant
  to pass it to --only-packages.
  [1]
