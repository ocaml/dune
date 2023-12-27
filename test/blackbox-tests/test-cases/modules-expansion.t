Here we test the ability of (modules) to be contain dynamic forms such as
`(:include)` and variables such as `"%{read-lines:}"`.

Begin by setting up a project. Note that the feature is not currently versioned;
this should be done before merging.

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
  > (executable (name mod) (modules (:include gen/lst)))
  > EOF

Let's check that it works:

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
  > (executable (name mod) (modules (:include gen/lst)))
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
  >  (modules (:include gen/lst)))
  > EOF

  $ dune build --display short
        ocamlc .lib.objs/byte/lib.{cmi,cmo,cmt}
      ocamldep .lib.objs/lib__Mod.impl.d
      ocamlopt .lib.objs/native/lib.{cmx,o}
        ocamlc .lib.objs/byte/lib__Mod.{cmi,cmo,cmt}
      ocamlopt .lib.objs/native/lib__Mod.{cmx,o}
        ocamlc lib.cma
      ocamlopt lib.{a,cmxa}
      ocamlopt lib.cmxs

We can also use special forms such as `%{read-lines:}`:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modules %{read-lines:gen/lst}))
  > EOF

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo "mod\nmod2\n")))
  > EOF

  $ touch mod2.ml

  $ dune build --display short
        ocamlc .lib.objs/byte/lib.{cmi,cmo,cmt}
      ocamldep .lib.objs/lib__Mod2.impl.d
      ocamlopt .lib.objs/native/lib.{cmx,o}
        ocamlc .lib.objs/byte/lib__Mod.{cmi,cmo,cmt}
        ocamlc .lib.objs/byte/lib__Mod2.{cmi,cmo,cmt}
      ocamlopt .lib.objs/native/lib__Mod.{cmx,o}
      ocamlopt .lib.objs/native/lib__Mod2.{cmx,o}
        ocamlc lib.cma
      ocamlopt lib.{a,cmxa}
      ocamlopt lib.cmxs

Next, we illustrate the issue mentioned above: the build dependencies must not
live in the same directory as the containing stanza, otherwise a cycle
appears. We need to handle this cycle gracefully and report it to the user.

  $ cat >dune <<EOF
  > (rule (with-stdout-to lst (echo "mod")))
  > (executable (name mod) (modules (:include lst)))
  > EOF

  $ dune exec ./mod.exe
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("internal dependency cycle",
    { frames =
        [ ("build-file", In_build_dir "default/lst")
        ; ("<unnamed>", ())
        ; ("<unnamed>", ())
        ; ("load-dir", In_build_dir "default")
        ]
    })
  Raised at Memo.Exec.exec_dep_node.(fun) in file "src/memo/memo.ml", line
    1289, characters 29-62
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
