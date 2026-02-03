Note about versioning:

- in (lang dune 1.x), no formatting rules are set up by default.
- (lang dune 2.0) behaves as if (using fmt 1.2) is set.

Formatting can be checked using the @fmt target:

  $ touch .ocamlformat
  $ cat > enabled/ocaml_file.ml << EOF
  > let  y=()
  > EOF
  $ cat > enabled/reason_file.re << EOF
  > let  y = ();
  > EOF
  $ cat > enabled/dune << EOF
  > (library
  > 
  > (name
  > lib_reason
  > 
  > )
  > )
  > EOF
  $ echo '(lang dune 2.0)' > dune-project
  $ echo '(lang dune 2.0)' > lang2/default/dune-project
  $ cat > lang2/partial/dune-project << EOF
  > (lang dune 2.0)
  > 
  > (formatting
  >  (enabled_for ocaml))
  > EOF
  $ dune build @fmt
  File "enabled/dune", line 1, characters 0-0:
  --- enabled/dune
  +++ enabled/.formatted/dune
  @@ -1,7 +1,2 @@
   (library
  -
  -(name
  -lib_reason
  -
  -)
  -)
  + (name lib_reason))
  File "enabled/ocaml_file.ml", line 1, characters 0-0:
  --- enabled/ocaml_file.ml
  +++ enabled/.formatted/ocaml_file.ml
  @@ -1 +1,2 @@
  -let  y=()
  +Sys.argv: ../install/default/bin/ocamlformat --impl enabled/ocaml_file.ml
  +ocamlformat output
  File "enabled/ocaml_file.mli", line 1, characters 0-0:
  --- enabled/ocaml_file.mli
  +++ enabled/.formatted/ocaml_file.mli
  @@ -1,2 +1,2 @@
  -val y   :
  -  unit
  +Sys.argv: ../install/default/bin/ocamlformat --intf enabled/ocaml_file.mli
  +ocamlformat output
  File "enabled/reason_file.re", line 1, characters 0-0:
  --- enabled/reason_file.re
  +++ enabled/.formatted/reason_file.re
  @@ -1 +1,2 @@
  -let  y = ();
  +Sys.argv: ../install/default/bin/refmt enabled/reason_file.re
  +refmt output
  File "enabled/reason_file.rei", line 1, characters 0-0:
  --- enabled/reason_file.rei
  +++ enabled/.formatted/reason_file.rei
  @@ -1 +1,2 @@
  -let y : unit;
  +Sys.argv: ../install/default/bin/refmt enabled/reason_file.rei
  +refmt output
  File "enabled/subdir/dune", line 1, characters 0-0:
  --- enabled/subdir/dune
  +++ enabled/subdir/.formatted/dune
  @@ -1,3 +1,2 @@
   (library
  - (name lib)
  -)
  + (name lib))
  File "enabled/subdir/lib.ml", line 1, characters 0-0:
  --- enabled/subdir/lib.ml
  +++ enabled/subdir/.formatted/lib.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl enabled/subdir/lib.ml
  +ocamlformat output
  File "lang2/default/dune", line 1, characters 0-0:
  --- lang2/default/dune
  +++ lang2/default/.formatted/dune
  @@ -1 +1,2 @@
  -(executable (name e))
  +(executable
  + (name e))
  File "lang2/default/e.ml", line 1, characters 0-0:
  --- lang2/default/e.ml
  +++ lang2/default/.formatted/e.ml
  @@ -1,3 +1,2 @@
  -let () =
  -  print_endline
  -    "e"
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/default/e.ml
  +ocamlformat output
  File "lang2/partial/a.ml", line 1, characters 0-0:
  --- lang2/partial/a.ml
  +++ lang2/partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/partial/a.ml
  +ocamlformat output
  File "partial/a.ml", line 1, characters 0-0:
  --- partial/a.ml
  +++ partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl partial/a.ml
  +ocamlformat output
  [1]

Configuration files are taken into account for this action:

  $ touch enabled/.ocamlformat
  $ dune build @fmt
  File "enabled/dune", line 1, characters 0-0:
  --- enabled/dune
  +++ enabled/.formatted/dune
  @@ -1,7 +1,2 @@
   (library
  -
  -(name
  -lib_reason
  -
  -)
  -)
  + (name lib_reason))
  File "enabled/ocaml_file.ml", line 1, characters 0-0:
  --- enabled/ocaml_file.ml
  +++ enabled/.formatted/ocaml_file.ml
  @@ -1 +1,2 @@
  -let  y=()
  +Sys.argv: ../install/default/bin/ocamlformat --impl enabled/ocaml_file.ml
  +ocamlformat output
  File "enabled/ocaml_file.mli", line 1, characters 0-0:
  --- enabled/ocaml_file.mli
  +++ enabled/.formatted/ocaml_file.mli
  @@ -1,2 +1,2 @@
  -val y   :
  -  unit
  +Sys.argv: ../install/default/bin/ocamlformat --intf enabled/ocaml_file.mli
  +ocamlformat output
  File "enabled/reason_file.re", line 1, characters 0-0:
  --- enabled/reason_file.re
  +++ enabled/.formatted/reason_file.re
  @@ -1 +1,2 @@
  -let  y = ();
  +Sys.argv: ../install/default/bin/refmt enabled/reason_file.re
  +refmt output
  File "enabled/reason_file.rei", line 1, characters 0-0:
  --- enabled/reason_file.rei
  +++ enabled/.formatted/reason_file.rei
  @@ -1 +1,2 @@
  -let y : unit;
  +Sys.argv: ../install/default/bin/refmt enabled/reason_file.rei
  +refmt output
  File "enabled/subdir/dune", line 1, characters 0-0:
  --- enabled/subdir/dune
  +++ enabled/subdir/.formatted/dune
  @@ -1,3 +1,2 @@
   (library
  - (name lib)
  -)
  + (name lib))
  File "enabled/subdir/lib.ml", line 1, characters 0-0:
  --- enabled/subdir/lib.ml
  +++ enabled/subdir/.formatted/lib.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl enabled/subdir/lib.ml
  +ocamlformat output
  File "lang2/default/dune", line 1, characters 0-0:
  --- lang2/default/dune
  +++ lang2/default/.formatted/dune
  @@ -1 +1,2 @@
  -(executable (name e))
  +(executable
  + (name e))
  File "lang2/default/e.ml", line 1, characters 0-0:
  --- lang2/default/e.ml
  +++ lang2/default/.formatted/e.ml
  @@ -1,3 +1,2 @@
  -let () =
  -  print_endline
  -    "e"
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/default/e.ml
  +ocamlformat output
  File "lang2/partial/a.ml", line 1, characters 0-0:
  --- lang2/partial/a.ml
  +++ lang2/partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/partial/a.ml
  +ocamlformat output
  File "partial/a.ml", line 1, characters 0-0:
  --- partial/a.ml
  +++ partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl partial/a.ml
  +ocamlformat output
  [1]

And fixable files can be promoted:

  $ dune promote enabled/ocaml_file.ml enabled/reason_file.re enabled/dune
  Promoting _build/default/enabled/.formatted/dune to enabled/dune.
  Promoting _build/default/enabled/.formatted/ocaml_file.ml to
    enabled/ocaml_file.ml.
  Promoting _build/default/enabled/.formatted/reason_file.re to
    enabled/reason_file.re.
  $ cat enabled/ocaml_file.ml
  Sys.argv: ../install/default/bin/ocamlformat --impl enabled/ocaml_file.ml
  ocamlformat output
  $ cat enabled/reason_file.re
  Sys.argv: ../install/default/bin/refmt enabled/reason_file.re
  refmt output
  $ cat enabled/dune
  (library
   (name lib_reason))

The fmt command automatically promotes the formatted files:

  $ touch fmt-cmd/.ocamlformat
  $ cat > fmt-cmd/ocaml_file.ml << EOF
  > let  y=()
  > EOF
  $ cat > fmt-cmd/reason_file.re << EOF
  > let  y = ();
  > EOF
  $ (cd fmt-cmd && dune fmt)
  $ cat fmt-cmd/ocaml_file.ml
  let  y=()
  $ cat fmt-cmd/reason_file.re
  let  y = ();

All .ocamlformat files are considered dependencies:

  $ echo 'margin = 70' > .ocamlformat
  $ dune build @fmt
  File "enabled/ocaml_file.mli", line 1, characters 0-0:
  --- enabled/ocaml_file.mli
  +++ enabled/.formatted/ocaml_file.mli
  @@ -1,2 +1,2 @@
  -val y   :
  -  unit
  +Sys.argv: ../install/default/bin/ocamlformat --intf enabled/ocaml_file.mli
  +ocamlformat output
  File "enabled/reason_file.rei", line 1, characters 0-0:
  --- enabled/reason_file.rei
  +++ enabled/.formatted/reason_file.rei
  @@ -1 +1,2 @@
  -let y : unit;
  +Sys.argv: ../install/default/bin/refmt enabled/reason_file.rei
  +refmt output
  File "enabled/subdir/dune", line 1, characters 0-0:
  --- enabled/subdir/dune
  +++ enabled/subdir/.formatted/dune
  @@ -1,3 +1,2 @@
   (library
  - (name lib)
  -)
  + (name lib))
  File "enabled/subdir/lib.ml", line 1, characters 0-0:
  --- enabled/subdir/lib.ml
  +++ enabled/subdir/.formatted/lib.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl enabled/subdir/lib.ml
  +ocamlformat output
  File "lang2/default/dune", line 1, characters 0-0:
  --- lang2/default/dune
  +++ lang2/default/.formatted/dune
  @@ -1 +1,2 @@
  -(executable (name e))
  +(executable
  + (name e))
  File "lang2/default/e.ml", line 1, characters 0-0:
  --- lang2/default/e.ml
  +++ lang2/default/.formatted/e.ml
  @@ -1,3 +1,2 @@
  -let () =
  -  print_endline
  -    "e"
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/default/e.ml
  +ocamlformat output
  File "lang2/partial/a.ml", line 1, characters 0-0:
  --- lang2/partial/a.ml
  +++ lang2/partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl lang2/partial/a.ml
  +ocamlformat output
  File "partial/a.ml", line 1, characters 0-0:
  --- partial/a.ml
  +++ partial/.formatted/a.ml
  @@ -1 +1,2 @@
  -let   x = 2
  +Sys.argv: ../install/default/bin/ocamlformat --impl partial/a.ml
  +ocamlformat output
  [1]

Mixing (lang dune 2.0) and (using fmt 1.0) is an error.
But a helpful message is displayed.

  $ cp lang2/partial/dune-project lang2/partial/dune-project.bak
  $ echo '(using fmt 1.0)' >> lang2/partial/dune-project
  $ dune build @lang2/partial/fmt
  File "lang2/partial/dune-project", line 5, characters 0-15:
  5 | (using fmt 1.0)
      ^^^^^^^^^^^^^^^
  Error: Starting with (lang dune 2.0), formatting is enabled by default.
  To port it to the new syntax, you can replace this part by:
  (formatting (enabled_for ocaml reason))
  [1]
  $ mv lang2/partial/dune-project.bak lang2/partial/dune-project

Sometimes, the suggestion is to just remove the configuration.

  $ echo '(using fmt 1.2)' >> lang2/partial/dune-project
  $ dune build @lang2/partial/fmt
  File "lang2/partial/dune-project", line 5, characters 0-15:
  5 | (using fmt 1.2)
      ^^^^^^^^^^^^^^^
  Error: Starting with (lang dune 2.0), formatting is enabled by default.
  To port it to the new syntax, you can delete this part.
  [1]

Formatting can also be set in the (env ...) stanza

  $ mkdir -p using-env
  $ cat >using-env/dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ cat >using-env/dune <<EOF
  > (env (_ (formatting disabled)))
  > EOF
  $ mkdir -p using-env/subdir
  $ cat >using-env/subdir/dune <<EOF
  > (executable (name foo))
  > EOF
  $ cat >using-env/subdir/foo.ml <<EOF
  > let x =     12
  > EOF
  $ (cd using-env && dune build @fmt)
  File "dune", line 1, characters 8-29:
  1 | (env (_ (formatting disabled)))
              ^^^^^^^^^^^^^^^^^^^^^
  Error: 'formatting' is only available since version 2.8 of the dune language.
  Please update your dune-project file to have (lang dune 2.8).
  [1]
  $ cat >using-env/dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ (cd using-env && dune build @fmt)
  $ cat >using-env/dune <<EOF
  > (env (_ (formatting (enabled_for ocaml))))
  > EOF
  $ touch using-env/.ocamlformat
  $ (cd using-env && dune build @fmt)
  fake ocamlformat is running: "--impl" "subdir/foo.ml"
  File "subdir/foo.ml", line 1, characters 0-0:
  --- subdir/foo.ml
  +++ subdir/.formatted/foo.ml
  @@ -1 +1 @@
  -let x =     12
  +(* fake ocamlformat output *)
  \ No newline at end of file
  [1]

We check that the formatting stanza in (env ...) takes precedence over that in
dune-project:

  $ cat >>using-env/dune-project <<EOF
  > (formatting disabled)
  > EOF
  $ (cd using-env && dune build @fmt)
  File "subdir/foo.ml", line 1, characters 0-0:
  --- subdir/foo.ml
  +++ subdir/.formatted/foo.ml
  @@ -1 +1 @@
  -let x =     12
  +(* fake ocamlformat output *)
  \ No newline at end of file
  [1]

Next we check that the new logic does not interfere with default per-project
settings as dictated by the dune language version.

  $ cat >using-env/subdir/dune-project <<EOF
  > (lang dune 1.7)
  > ;; formatting disabled by default
  > EOF
  $ (cd using-env && dune build @fmt)
