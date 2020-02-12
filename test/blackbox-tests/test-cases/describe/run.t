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

  $ dune describe workspace --lang 2.3
  [ library
      { name = "foo"
      ; uid = "0c8684c86d6ffbcecee09205033aa0a5"
      ; requires = [ "da24c54ea66d10c3bf80362265e8accb" ]
      ; source_dir = "_build/default"
      ; modules =
          [ { name = "Foo"
            ; impl = Some "_build/default/foo.ml"
            ; intf = None
            ; cmt = Some "_build/default/.foo.objs/byte/foo.cmt"
            ; cmti = None
            }
          ]
      }
  ; library
      { name = "foo.x"
      ; uid = "da24c54ea66d10c3bf80362265e8accb"
      ; requires = []
      ; source_dir = "_build/default"
      ; modules =
          [ { name = "Foo_x"
            ; impl = Some "_build/default/foo_x.ml"
            ; intf = None
            ; cmt = Some "_build/default/.foo_x.objs/byte/foo_x.cmt"
            ; cmti = None
            }
          ]
      }
  ]

Test other formats
------------------

  $ dune describe workspace --format csexp --lang 2.3 | cut -c 1-70
  ((7:library((4:name3:foo)(3:uid32:0c8684c86d6ffbcecee09205033aa0a5)(8:

Test errors
-----------

  $ dune describe --lang 2.3 workspac
  Error: Unknown constructor workspac
  Hint: did you mean workspace?
  [1]

  $ dune describe --lang 2.3 workspace xxx
  Error: Too many argument for workspace
  [1]
