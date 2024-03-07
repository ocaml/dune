Test that we can write config parameters in both the configuration
file and the workspace file.

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune<<EOF
  > (rule
  >  (alias default)
  >  (action (run echo "Hello, world!")))
  > EOF

Setting such options is not supported with older Dune:

  $ cat >dune-workspace<<EOF
  > (lang dune 2.8)
  > (display short)
  > EOF
  $ dune build -f
  File "dune-workspace", line 2, characters 0-15:
  2 | (display short)
      ^^^^^^^^^^^^^^^
  Error: 'display' is only available since version 3.0 of the dune language.
  Please update your dune-project file to have (lang dune 3.0).
  [1]

But is supported with Dune >= 3.0.0:

  $ cat >dune-workspace<<EOF
  > (lang dune 3.0)
  > (display short)
  > EOF
  $ dune build -f
          echo alias default
  Hello, world!

  $ cat >dune-workspace<<EOF
  > (lang dune 3.0)
  > (display verbose)
  > EOF
  $ dune build -f 2>&1 | grep Hello | sed 's/&&.*echo/\&\& echo/'
  Running[1]: (cd _build/default && echo 'Hello, world!')
  Hello, world!

Make sure errors related to fields other than the ones allowed in the
config field are not reported if we don't need to evaluate these
fields:

  $ mkdir errors
  $ cd errors

  $ cat >dune-workspace<<EOF
  > (lang dune 3.0)
  > (context (blah))
  > EOF

  $ dune init project foo
  Entering directory 'foo'
  Success: initialized project component named foo
  Leaving directory 'foo'

But if we do the build we do get an error:

  $ dune build
  File "dune-workspace", line 2, characters 10-14:
  2 | (context (blah))
                ^^^^
  Error: Unknown constructor blah
  [1]

