  $ echo titi > x

  $ $JBUILDER build --root . -j1 --diff-command false @blah
            sh (internal) (exit 1)
  /usr/bin/sh -c 'false '\''x'\'' '\''_build/default/x.gen'\'''
  [1]
  $ cat x
  titi

  $ $JBUILDER build --root . -j1 --diff-command false @blah --promote ignore
  $ cat x
  titi

  $ $JBUILDER build --root . -j1 --diff-command false @blah --promote copy
            sh (internal) (exit 1)
  /usr/bin/sh -c 'false '\''x'\'' '\''_build/default/x.gen'\'''
  Promoting _build/default/x.gen to x.
  [1]
  $ cat x
  toto
  $ $JBUILDER build --root . -j1 --diff-command false @blah --promote copy
