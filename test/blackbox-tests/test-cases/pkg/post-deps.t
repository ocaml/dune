Post dependency cycle handling

  $ . ./helpers.sh

  $ make_lockdir

Simplest case:

  $ cat >dune.lock/a.pkg <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ cat >dune.lock/b.pkg <<EOF
  > (version 0.0.1)
  > (post_depends a)
  > EOF

  $ build_pkg a

  $ rm -rf dune.lock
  $ make_lockdir

More diabolical case.

  $ cat >dune.lock/a.pkg <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF

  $ cat >dune.lock/b.pkg <<EOF
  > (version 0.0.1)
  > (post_depends c)
  > EOF

  $ cat >dune.lock/c.pkg <<EOF
  > (version 0.0.1)
  > (post_depends a)
  > EOF

  $ build_pkg a
