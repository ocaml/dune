  $ printf titi > x

  $ jbuilder build --display short --diff-command false @blah 2>&1 | sed 's/.*false.*/DIFF/'
            sh (internal) (exit 1)
  DIFF
  $ cat x
  titi

  $ jbuilder promote --display short
  Promoting _build/default/x.gen to x.
  $ cat x
  toto

  $ jbuilder build --display short --diff-command false @blah
  $ cat x
  toto

Otherwise this test fails on OSX
  $ jbuilder clean --display short

  $ printf titi > x
  $ jbuilder build --display short --diff-command false @blah --auto-promote 2>&1 | sed 's/.*false.*/DIFF/'
            sh (internal) (exit 1)
  DIFF
  Promoting _build/default/x.gen to x.
  $ cat x
  toto
  $ jbuilder build --display short --diff-command false @blah
  $ cat x
  toto
