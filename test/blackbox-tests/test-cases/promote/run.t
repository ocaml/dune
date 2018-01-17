  $ echo titi > x

  $ $JBUILDER build --root . -j1 --diff-command false @blah
            sh (internal) (exit 1)
  (cd _build/default && /usr/bin/sh -c 'false x x.gen')
  [1]
  $ cat x
  titi

  $ $JBUILDER promote --root .
  Promoting _build/default/x.gen to x.
  $ cat x
  toto

  $ $JBUILDER build --root . -j1 --diff-command false @blah
  $ cat x
  toto

  $ echo titi > x
  $ $JBUILDER build --root . -j1 --diff-command false @blah --auto-promote
            sh (internal) (exit 1)
  (cd _build/default && /usr/bin/sh -c 'false x x.gen')
  Promoting _build/default/x.gen to x.
  [1]
  $ cat x
  toto
  $ $JBUILDER build --root . -j1 --diff-command false @blah
  $ cat x
  toto
