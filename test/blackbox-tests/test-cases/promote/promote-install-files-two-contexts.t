Regression test against https://github.com/ocaml/dune/pull/16983

`dune build -p` promotes the install file of every build context to the same
path in the source tree. After https://github.com/ocaml/dune/pull/15559 the
promotions from different contexts run concurrently and would write results
to the same temporary file, `.#<pkg>.install.dune-temp` before effecting the
promotion.  This set up a race condition between contexts, and in some
interleavings the build of one context will have written to the temp file
before it is moved by the other, and then finds the file missing when it tries to
promote.

Set up a workspace with two contexts:

  $ make_dune_project_with_package 3.0 foo
  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (context (default (name one)))
  > (context (default (name another)))
  > EOF

Define a rule that will install a file:

  $ cat > dune <<EOF
  > (install (section share) (files hello.txt))
  > EOF
  $ echo hi > hello.txt

The race condition would causes errors often, but not always, so we run the build
multiple times to ensure we get at least one failure. This should always succeed
without error:

  $ for i in 1 2 3; do dune clean; dune build -p foo @install 2>&1; done
