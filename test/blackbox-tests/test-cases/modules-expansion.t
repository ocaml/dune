Here we test the ability of (modules) to be contain dynamic forms such as
`(:include)` and variables such as `"%{read-lines:}"`.

Begin by setting up a project and check the versioning guards.

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

As we will see later in the test, it is imperative that build dependencies
needed to evaluate the `(modules)` field not live in the same directory as the
containing stanza. We will put them in a subdirectory:

  $ mkdir -p gen

We define a rule that creates a file (in sexp syntax, to be passed to
`(:include)`) containing a single name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo mod)))
  > EOF

The unit `mod.ml` is present in the working tree:

  $ cat >mod.ml <<EOF
  > let () = print_endline "Hello, Mod!"
  > EOF

We declare a `executable` where the list of modules is read from the (generated)
file `gen/lst`:

  $ cat >dune <<EOF
  > (executable (name mod) (modes byte) (modules (:include gen/lst)))
  > EOF

Let's check that it fails in the current version of Dune:

  $ dune exec ./mod.exe
  File "dune", line 1, characters 36-64:
  1 | (executable (name mod) (modes byte) (modules (:include gen/lst)))
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: the ability to specify non-constant module lists is only available
  since version 3.13 of the dune language. Please update your dune-project file
  to have (lang dune 3.13).
  [1]

Update the version...

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

... and it works!

  $ dune exec ./mod.exe
  Hello, Mod!

Let's check that error messages owning to non-existent modules continue to work:

  $ cat >dune <<EOF
  > (executable (name does_not_exist) (modules (:include gen/lst)))
  > EOF

  $ dune exec ./does_not_exist.exe
  File "dune", line 1, characters 18-32:
  1 | (executable (name does_not_exist) (modules (:include gen/lst)))
                        ^^^^^^^^^^^^^^
  Error: The name "Does_not_exist" is not listed in the (modules) field of this
  stanza.
  [1]

  $ cat >dune <<EOF
  > (executable (name mod) (modules does_not_exist (:include gen/lst)))
  > EOF

  $ dune exec ./mod.exe
  File "dune", line 1, characters 32-46:
  1 | (executable (name mod) (modules does_not_exist (:include gen/lst)))
                                      ^^^^^^^^^^^^^^
  Error: Module Does_not_exist doesn't exist.
  [1]

  $ mv mod.ml mod2.ml
  $ cat >dune <<EOF
  > (executable (name mod) (modes byte) (modules (:include gen/lst)))
  > EOF

Locations are accurate even for generated files:

  $ dune exec ./mod.exe
  File "_build/default/gen/lst", line 1, characters 0-3:
  1 | mod
      ^^^
  Error: Module Mod doesn't exist.
  [1]

  $ mv mod2.ml mod.ml

Let's do some examples using libraries:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules (:include gen/lst)))
  > EOF

  $ dune build lib.cma

We can also use special forms such as `%{read-lines:}`:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules %{read-lines:gen/lst}))
  > EOF

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo "mod\nmod2\n")))
  > EOF

  $ touch mod2.ml

  $ dune build lib.cma

Interaction with `(include_subdirs)` when the dependencies are in the subtree:

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules %{read-lines:gen/lst}))
  > EOF

  $ dune build lib.cma
  Error: Dependency cycle between:
     (modules) field at dune:2
  -> %{read-lines:gen/lst} at dune:5
  -> (modules) field at dune:2
  [1]

Let's move the gen subdirectory out of the hierarchy:

  $ rm dune
  $ mkdir -p lib/sub
  $ cat >lib/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name lib2)
  >  (modes byte)
  >  (modules %{read-lines:../gen/lst}))
  > EOF

  $ cp mod.ml lib
  $ cp mod2.ml lib/sub

  $ dune build lib/lib2.cma

  $ rm -rf lib

Next, we illustrate the issue mentioned above: the build dependencies must not
live in the same directory as the containing stanza, otherwise a cycle
appears. We need to handle this cycle gracefully and report it to the user.

  $ cat >dune <<EOF
  > (rule (with-stdout-to lst (echo "mod")))
  > (executable (name mod) (modes byte) (modules (:include lst)))
  > EOF

  $ dune exec ./mod.exe
  Error: Dependency cycle between:
     (modules) field at dune:2
  -> (:include _build/default/lst) at dune:2
  -> (modules) field at dune:2
  [1]

Let's do one example with a generated source file:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules mod3 (:include gen/lst)))
  > (rule (with-stdout-to mod3.ml (progn)))
  > (rule (with-stdout-to mod4.ml (progn)))
  > EOF

  $ echo mod4 >gen/lst && rm -f gen/dune mod.ml mod2.ml

  $ dune build lib.cma
