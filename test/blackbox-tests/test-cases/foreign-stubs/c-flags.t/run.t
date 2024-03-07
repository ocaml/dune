Prior to dune 2.8 ocamlc_cflags and ocamlc_cppflags where
always prepended to the C compiler command line

In the following tests, foo.c is built with the :standard set of flags while
bar.c is built with an "empty" set of flags.

  $ O_CC=$(ocamlc -config-var c_compiler)
  $ O_CCF=$(ocamlc -config-var ocamlc_cflags)
  $ O_CCPPF=$(ocamlc -config-var ocamlc_cppflags)
  $ O_CC=$(echo $O_CC | sed -e 's/^[ \t]*//')
  $ O_CCF=$(echo $O_CCF | sed -e 's/^[ \t]*//')
  $ O_CCPPF=$(echo $O_CCPPF | sed -e 's/^[ \t]*//')



use_standard_c_and_cxx_flags = default (false)
==================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune rules -m foo.o | tr -s '\t\n\\' ' ' > out_foo
  File "dune", line 4, characters 36-39:
  4 |  (foreign_stubs (language c) (names bar) (flags)))
                                          ^^^
  Warning: The flag set for these foreign sources overrides the `:standard` set
  of flags. However the flags in this standard set are still added to the
  compiler arguments by Dune. This might cause unexpected issues. You can
  disable this warning by defining the option `(use_standard_c_and_cxx_flags
  <bool>)` in your `dune-project` file. Setting this option to `true` will
  effectively prevent Dune from silently adding c-flags to the compiler
  arguments which is the new recommended behaviour.

  $ dune rules -m bar.o | tr -s '\t\n\\' ' ' > out_bar
  File "dune", line 4, characters 36-39:
  4 |  (foreign_stubs (language c) (names bar) (flags)))
                                          ^^^
  Warning: The flag set for these foreign sources overrides the `:standard` set
  of flags. However the flags in this standard set are still added to the
  compiler arguments by Dune. This might cause unexpected issues. You can
  disable this warning by defining the option `(use_standard_c_and_cxx_flags
  <bool>)` in your `dune-project` file. Setting this option to `true` will
  effectively prevent Dune from silently adding c-flags to the compiler
  arguments which is the new recommended behaviour.

No warning in vendored subfolder

  $ dune build vendor/barv.o

Ocamlc_cflags are duplicated if the :standard set is kept:
  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF} ${O_CCF}"
  1

Whether or not the :standard flags is overridden, both ocamlc_cflags and
ocamlc_cpp flags appear in the compiler command line:

  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF}"
  1

  $ cat out_bar | grep -ce "${O_CCF} ${O_CCPPF}"
  1

use_standard_c_and_cxx_flags = true
=================================

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

  $ dune rules -m foo.o | tr -s '\t\n\\' ' ' > out_foo
  $ dune rules -m bar.o | tr -s '\t\n\\' ' ' > out_bar

Ocamlc_cflags are not duplicated anymore:
  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF} ${O_CCF}"
  0
  [1]

When the :standard flags is overridden, ocamlc_cflags and
ocamlc_cpp are effectively removed from the compiler command line

  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF}"
  1

  $ cat out_bar | grep -ce "${O_CCF}"
  0
  [1]

  $ cat out_bar | grep -ce "${O_CCPPF}"
  0
  [1]

use_standard_c_and_cxx_flags = true but dune < 2.8
================================================

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (use_standard_c_and_cxx_flags true)
  > EOF

  $ dune rules
  File "dune-project", line 2, characters 0-35:
  2 | (use_standard_c_and_cxx_flags true)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'use_standard_c_and_cxx_flags' is only available since version 2.8 of
  the dune language. Please update your dune-project file to have (lang dune
  2.8).
  [1]

