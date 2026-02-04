Test for (sandbox patch_back_source_tree)

This sandbox allows to safely "modify" source files by turning modifications
into promotions.

  $ cat >dune-project<<EOF
  > (lang dune 3.22)
  > EOF

Targest are not promoted
------------------------

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree))
  >  (targets x)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ dune build x
  $ dune promote
  $ if [[ -f x ]]; then echo promoted; else echo not promoted; fi
  not promoted

All modified dependencies are promoted
--------------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (deps x (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ echo blah > x
  $ dune build

  $ dune trace cat | jq '
  > include "dune";
  >   select(.cat == "sandbox" and .name == "snapshot")
  > | censorDigestDir
  > | .args
  > '
  {
    "loc": "dune:1",
    "dir": "_build/.sandbox/$DIGEST"
  }
  {
    "loc": "dune:1",
    "dir": "_build/.sandbox/$DIGEST"
  }

  $ dune promote x
  Promoting _build/default/x to x.
  $ cat x
  Hello, world!

Non-modified dependencies are not promoted
------------------------------------------

  $ rm -f x
  $ cat >dune<<EOF
  > (rule
  >  (alias default)
  >  (deps x (sandbox patch_back_source_tree))
  >  (action (system "echo 'Hello, world!'")))
  > (rule (with-stdout-to x (progn)))
  > EOF

  $ dune build
  Hello, world!
  $ dune promotion list

All other new files are copied
------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "echo 'Hello, world!' > y")))
  > EOF

  $ dune build
  $ dune promote
  Promoting _build/default/y to y.
  $ cat y
  Hello, world!

Directories are created if needed
---------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "mkdir z; echo 'Hello, world!' > z/z")))
  > EOF

  $ dune build
  $ dune promote
  Promoting _build/default/z/z to z/z.
  $ cat z/z
  Hello, world!

Actions are allowed to delete files
-----------------------------------

  $ touch foo

  $ cat >dune<<EOF
  > (rule
  >  (deps foo (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "rm foo")))
  > EOF

  $ dune build
  $ dune promote
  $ [[ ! -f foo ]] && echo foo has been deleted
  foo has been deleted

Actions are allowed to delete directories
-----------------------------------------

  $ mkdir -p todelete/y/
  $ touch todelete/y/foo

  $ cat >dune<<EOF
  > (rule
  >  (deps todelete/y/foo (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "rm -rf todelete")))
  > EOF

  $ dune build

  $ dune promote
  $ [[ ! -d todelete ]] && echo todelete has been deleted
  todelete has been deleted

Actions are allowed to change directories into files
----------------------------------------------------

  $ mkdir -p dir/y/
  $ touch dir/y/foo

  $ cat >dune<<EOF
  > (rule
  >  (deps dir/y/foo (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "rm -rf dir && echo foo > dir")))
  > EOF

  $ dune promote
  $ [[ -f dir ]] && cat dir
  [1]
  $ [[ -d dir ]] && echo still a directory
  still a directory

Interaction with explicit sandboxing
------------------------------------

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree) (sandbox none))
  >  (alias default)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune build
  File "dune", lines 1-4, characters 0-121:
  1 | (rule
  2 |  (deps (sandbox patch_back_source_tree) (sandbox none))
  3 |  (alias default)
  4 |  (action (system "echo 'Hello, world!'")))
  Error: This rule forbids all sandboxing modes (but it also requires
  sandboxing)
  [1]

Selecting an explicit sandbox mode via the command line doesn't affect
the rule:

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree))
  >  (alias default)
  >  (action (system "echo 'Hello, world!' > x")))
  > EOF

  $ test_with ()
  > {
  >   rm -f x
  >   dune clean
  >   dune build --sandbox $1
  >   dune promote
  >   cat x
  > }

  $ test_with copy
  Promoting _build/default/x to x.
  Hello, world!
  $ test_with hardlink
  Promoting _build/default/x to x.
  Hello, world!
  $ test_with symlink
  Promoting _build/default/x to x.
  Hello, world!

Interaction with files writable status
--------------------------------------

If a source file is read-only, the action sees it as writable:

  $ cat >dune<<EOF
  > (rule
  >  (deps x (sandbox patch_back_source_tree))
  >  (alias default)
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

  $ dune promote
  Promoting _build/default/x to x.

  $ if test -w x; then echo writable; else echo non-writable; fi
  writable

Reproduction case for copying the action stamp file
---------------------------------------------------

There used to be a bug causing the internal action stamp file to be
produced in the sandbox and copied back:

  $ cat >dune<<EOF
  > (rule
  >  (deps (sandbox patch_back_source_tree))
  >  (alias blah)
  >  (action (system "echo 'Hello, world!'")))
  > EOF

  $ dune build @blah
  Hello, world!

This is the internal stamp file:

  $ ls _build/.actions/default/blah* | dune_cmd subst '/blah-.+' '/blah-REDACTED'
  _build/.actions/default/blah-REDACTED
