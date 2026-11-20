`dune build -p` promotes the install file of every build context to the same
path in the source tree. Since https://github.com/ocaml/dune/pull/15559 the
promotions run concurrently and write results to the same temporary file,
`.#<pkg>.install.dune-temp` befor effecting the promotion. This sets up a race
condition between contexts, and in some interleaveings the build of one context
will have written to the temp file before it is moved by the other, and then
find the file missing when it tries to promote. This failure was caught in the
melange-fest.0.2.0 in the 3.25.0~alpha1 revdeps run for the prerelease
https://github.com/ocaml/opam-repository/pull/31082.

Set up a workspace with two contexts:

  $ make_dune_project_with_package 3.0 foo
  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (context (default (name one)))
  > (context (default (name another)))
  > EOF
  $ cat > dune <<EOF
  > (install (section share) (files hello.txt))
  > EOF
  $ echo hi > hello.txt

The race condition causes errors often, but not alwyas, so we run the build
multiple times to ensure we get at least one fialure. We only emit the error
line, because which either context can fail.

  $ for i in 1 2 3; do dune clean; dune build -p foo @install 2>&1; done | grep Error | head -n 1
  Error: rename(.#foo.install.dune-temp): No such file or directory
  [1]
