  $ echo titi > x

  $ $JBUILDER build --root . -j1 --diff-command false @blah 2>&1 | sed 's/.*false.*/DIFF/'
            sh (internal) (exit 1)
  DIFF
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
  $ $JBUILDER build --root . -j1 --diff-command false @blah --auto-promote 2>&1 | sed 's/.*false.*/DIFF/'
            sh (internal) (exit 1)
  DIFF
  Promoting _build/default/x.gen to x.
  $ cat x
  toto
  $ $JBUILDER build --root . -j1 --diff-command false @blah
  $ cat x
  toto
