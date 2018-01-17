  $ echo titi > x

  $ $JBUILDER build --root . -j1 --diff-command false @blah
            sh (internal) (exit 1)
  /usr/bin/sh -c 'false '\''_build/default/x'\'' '\''_build/default/x.gen'\'''
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
  /usr/bin/sh -c 'false '\''_build/default/x'\'' '\''_build/default/x.gen'\'''
  Promoting _build/default/x.gen to x.
  [1]
  $ cat x
  toto
  $ $JBUILDER build --root . -j1 --diff-command false @blah
  $ cat x
  toto
