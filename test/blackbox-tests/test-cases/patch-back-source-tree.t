Test for (mode patch-back-source-tree)

It's experimental and requires enabling explicitly
--------------------------------------------------

  $ cat >dune-project<<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (action (with-stdout-to x (progn))))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-29:
  2 |  (mode patch-back-source-tree)
             ^^^^^^^^^^^^^^^^^^^^^^
  Error: 'patch-back-source-tree' is available only when patch-back-source-tree
  is enabled in the dune-project file. You must enable it using (using
  patch-back-source-tree 0.1) in your dune-project file.
  Note however that patch-back-source-tree is experimental and might change
  without notice in the future.
  [1]

-----

  $ cat >dune-project<<EOF
  > (lang dune 3.0)
  > (using patch-back-source-tree 0.1)
  > EOF

All targets are promoted
------------------------

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (targets x)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ dune build x
  $ cat x
  Hello, world!

All modified dependencies are promoted
--------------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (deps x)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ echo blah > x
  $ dune build
  $ cat x
  Hello, world!

Non-modified dependencies are not promoted
------------------------------------------

  $ rm -f x
  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (deps x)
  >  (action (system "echo 'Hello, world!'")))
  > (rule (with-stdout-to x (progn)))
  > EOF

  $ dune build
  Hello, world!
  $ if ! test -f x; then echo ok; fi
  ok

All other new files are copied
------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (action (system "echo 'Hello, world!' > y")))
  > EOF

  $ dune build
  $ cat y
  Hello, world!

Directories are created if needed
---------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (action (system "mkdir z; echo 'Hello, world!' > z/z")))
  > EOF

  $ dune build
  $ cat z/z
  Hello, world!

Interaction with explicit sandboxing
------------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (deps (sandbox none))
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune build
  File "dune", lines 1-5, characters 0-119:
  1 | (rule
  2 |  (mode patch-back-source-tree)
  3 |  (deps (sandbox none))
  4 |  (alias default)
  5 |  (action (system "echo 'Hello, world!'")))
  Error: Rules with (mode patch-back-source-tree) cannot have an explicit
  sandbox configuration because it is implied by (mode patch-back-source-tree).
  [1]

Selecting an explicit sandbox mode via the command line doesn't affect
the rule:

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ test_with ()
  > {
  >   rm -f x
  >   dune clean
  >   dune build --sandbox $1
  >   cat x
  > }

  $ test_with copy
  Hello, world!
  $ test_with hardlink
  Hello, world!
  $ test_with symlink
  Hello, world!

Interaction with files writable status
--------------------------------------

If a source file is read-only, the action sees it as writable:

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias default)
  >  (deps x)
  >  (action (system "if test -w x; then echo writable; else echo non-writable; fi; echo blah > x")))
  > EOF

  $ echo xx > x
  $ chmod -w x

  $ if test -w x; then echo writable; else echo non-writable; fi
  non-writable

  $ dune build
  writable

And as the action modified `x`, its permissions have now changed
inside the source tree:

  $ if test -w x; then echo writable; else echo non-writable; fi
  writable

Reproduction case for copying the action stamp file
---------------------------------------------------

There used to be a bug causing the internal action stamp file to be
produced in the sandbox and copied back:

  $ cat >dune<<EOF
  > (rule
  >  (mode patch-back-source-tree)
  >  (alias blah)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune build @blah
  Hello, world!

This is the internal stamp file:

  $ ls _build/.actions/default/blah*
  _build/.actions/default/blah-98b715ceb840414fcdd2546357b09ca0

And we check that it isn't copied in the source tree:

  $ if [ -d default ]; then echo "Failure"; else echo "Success"; fi
  Success
