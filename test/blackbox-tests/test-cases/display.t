Document how Dune displays various things
=========================================

  $ echo '(lang dune 3.0)' > dune-project
  $ export BUILD_PATH_PREFIX_MAP=SH=`command -v sh`

Errors with location embed in their output
------------------------------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "echo 'File \"foo\", line 1: blah'; exit 42")))
  > EOF

  $ dune clean; dune build
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --always-show-command-line
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42')
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --display short
            sh alias default (exit 42)
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --display short --always-show-command-line
            sh alias default (exit 42)
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42')
  File "foo", line 1: blah
  [1]

Errors without location embed in their output
---------------------------------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "echo failure; exit 42")))
  > EOF

  $ dune clean; dune build
  File "dune", lines 1-3, characters 0-66:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo failure; exit 42")))
  failure
  [1]

  $ dune clean; dune build --always-show-command-line
  File "dune", lines 1-3, characters 0-66:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo failure; exit 42")))
  (cd _build/default && SH -c 'echo failure; exit 42')
  failure
  [1]

  $ dune clean; dune build --display short
  File "dune", lines 1-3, characters 0-66:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo failure; exit 42")))
            sh alias default (exit 42)
  failure
  [1]

  $ dune clean; dune build --display short --always-show-command-line
  File "dune", lines 1-3, characters 0-66:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "echo failure; exit 42")))
            sh alias default (exit 42)
  (cd _build/default && SH -c 'echo failure; exit 42')
  failure
  [1]

Errors with no output
---------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "exit 42")))
  > EOF

  $ dune clean; dune build
  File "dune", lines 1-3, characters 0-52:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "exit 42")))
  Command exited with code 42.
  [1]

  $ dune clean; dune build --always-show-command-line
  File "dune", lines 1-3, characters 0-52:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "exit 42")))
  (cd _build/default && SH -c 'exit 42')
  Command exited with code 42.
  [1]

  $ dune clean; dune build --display short
  File "dune", lines 1-3, characters 0-52:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "exit 42")))
            sh alias default (exit 42)
  [1]

  $ dune clean; dune build --display short --always-show-command-line
  File "dune", lines 1-3, characters 0-52:
  1 | (rule
  2 |  (alias default)
  3 |  (action (system "exit 42")))
            sh alias default (exit 42)
  (cd _build/default && SH -c 'exit 42')
  [1]

Successful commands with output
-------------------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune clean; dune build
  Hello, world!

  $ dune clean; dune build --always-show-command-line
  (cd _build/default && SH -c 'echo '\''Hello, world!'\''')
  Hello, world!

  $ dune clean; dune build --display short
            sh alias default
  Hello, world!

  $ dune clean; dune build --display short --always-show-command-line
            sh alias default
  (cd _build/default && SH -c 'echo '\''Hello, world!'\''')
  Hello, world!

Errors with-stdout-to
---------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (with-stdout-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  > EOF

  $ dune clean; dune build
  File "dune", lines 1-3, characters 0-108:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-stdout-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  Command exited with code 42.
  [1]

  $ dune clean; dune build --always-show-command-line
  File "dune", lines 1-3, characters 0-108:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-stdout-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') > _build/default/bar
  Command exited with code 42.
  [1]

  $ dune clean; dune build --display short
  File "dune", lines 1-3, characters 0-108:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-stdout-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
            sh bar (exit 42)
  [1]

  $ dune clean; dune build --display short --always-show-command-line
  File "dune", lines 1-3, characters 0-108:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-stdout-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
            sh bar (exit 42)
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') > _build/default/bar
  [1]

Errors with-stderr-to
---------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (with-stderr-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  > EOF

  $ dune clean; dune build
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --always-show-command-line
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') 2> _build/default/bar
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --display short
            sh bar (exit 42)
  File "foo", line 1: blah
  [1]

  $ dune clean; dune build --display short --always-show-command-line
            sh bar (exit 42)
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') 2> _build/default/bar
  File "foo", line 1: blah
  [1]

Errors with-outputs-to
---------------------

  $ cat >dune<<"EOF"
  > (rule
  >  (alias default)
  >  (action (with-outputs-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  > EOF

  $ dune clean; dune build
  File "dune", lines 1-3, characters 0-109:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-outputs-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  Command exited with code 42.
  [1]

  $ dune clean; dune build --always-show-command-line
  File "dune", lines 1-3, characters 0-109:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-outputs-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') &> _build/default/bar
  Command exited with code 42.
  [1]

  $ dune clean; dune build --display short
  File "dune", lines 1-3, characters 0-109:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-outputs-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
            sh bar (exit 42)
  [1]

  $ dune clean; dune build --display short --always-show-command-line
  File "dune", lines 1-3, characters 0-109:
  1 | (rule
  2 |  (alias default)
  3 |  (action (with-outputs-to bar (system "echo 'File \"foo\", line 1: blah'; exit 42"))))
            sh bar (exit 42)
  (cd _build/default && SH -c 'echo '\''File "foo", line 1: blah'\''; exit 42') &> _build/default/bar
  [1]
