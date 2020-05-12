Test for the `dune describe` command
====================================

Setup
-----

  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foo)
  >  (libraries foo.x)
  >  (modules foo))
  > 
  > (library
  >  (name foo_x)
  >  (public_name foo.x)
  >  (modules foo_x))
  > EOF

  $ touch foo.ml
  $ touch foo_x.ml

Describe various things
-----------------------

  $ dune describe workspace --lang 0.1
  ((library
    ((name foo)
     (uid 5dd4bd87ad37b4f5713085aff4bee9c9)
     (local true)
     (requires (c17373aee51bab94097b4b7818553cf3))
     (source_dir _build/default)
     (modules
       (((name Foo)
         (impl (_build/default/foo.ml))
         (intf ())
         (cmt (_build/default/.foo.objs/byte/foo.cmt))
         (cmti ()))))
     (include_dirs (_build/default/.foo.objs/byte))))
   (library
    ((name foo.x)
     (uid c17373aee51bab94097b4b7818553cf3)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
       (((name Foo_x)
         (impl (_build/default/foo_x.ml))
         (intf ())
         (cmt (_build/default/.foo_x.objs/byte/foo_x.cmt))
         (cmti ()))))
     (include_dirs (_build/default/.foo_x.objs/byte)))))

Test other formats
------------------

  $ dune describe workspace --format csexp --lang 0.1 | cut -c 1-85
  ((7:library((4:name3:foo)(3:uid32:5dd4bd87ad37b4f5713085aff4bee9c9)(5:local4:true)(8:

Test errors
-----------

  $ dune describe --lang 0.1 workspac
  Error: Unknown constructor workspac
  Hint: did you mean workspace?
  [1]

  $ dune describe --lang 0.1 workspace xxx
  Error: Too many argument for workspace
  [1]

  $ dune describe --lang 1.0
  dune: Only --lang 0.1 is available at the moment as this command is not yet
        stabilised. If you would like to release a software that relies on the
        output of 'dune describe', please open a ticket on
        https://github.com/ocaml/dune.
  Usage: dune describe [OPTION]... [STRING]...
  Try `dune describe --help' or `dune --help' for more information.
  [1]
