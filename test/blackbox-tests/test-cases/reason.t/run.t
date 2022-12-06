Tests for reason

Build and run a reason binary:

  $ dune build @runtest
  Cppome
  hello world
  Bar
  Foo

We make sure to install reason source files:

  $ dune build @install-file 2>&1 | grep ".re"
    "_build/install/default/lib/rlib/bar.re"
    "_build/install/default/lib/rlib/cppome.re"
    "_build/install/default/lib/rlib/cppome.rei"
    "_build/install/default/lib/rlib/foo.rei"
    "_build/install/default/lib/rlib/hello.re"
    "_build/install/default/lib/rlib/hello.rei"
    "_build/install/default/lib/rlib/pped.re"
    "_build/install/default/lib/rlib/pped.rei"
    "_build/install/default/bin/refmt"

virtual libraries in reason
  $ PATH="_build/install/default/bin:$PATH" dune build --root vlib-impl @all
  Entering directory 'vlib-impl'
  Leaving directory 'vlib-impl'
