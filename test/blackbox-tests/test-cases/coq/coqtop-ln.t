Running the Coq Toplevel with an absolute path containing a symbolic link.
This addresses a failure case reported by @MackieLoeffel in #5457 (see
https://github.com/ocaml/dune/pull/5457#issuecomment-1084149057).

  $ mkdir dir
  $ ln -s dir sl
  $ cat >dir/foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ cat >dir/dune-project <<EOF
  > (lang dune 3.0)
  > (using coq 0.3)
  > EOF
  $ cat >dir/dune <<EOF
  > (coq.theory
  >  (name basic))
  > EOF
  $ cd sl && dune coq top $PWD/foo.v > /dev/null 2> /dev/null
