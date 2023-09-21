  $ . ./opam-var-helpers.sh

Here we test the translation and implementation of opam package variables.

We echo each package variable. 

  $ mkrepo
  > mkpkg "testpkg" <<'EOF' 
  > opam-version: "2.0"
  > depends: [ "foo" ]
  > build: [
  >   [ "echo" "1"     name ]
  >   [ "echo" "2"   _:name ]
  >   [ "echo" "3" foo:name ]
  >   [ "echo" "4"     version ]
  >   [ "echo" "5"   _:version ]
  >   [ "echo" "6" foo:version ]
  >   [ "echo" "7"   _:depends ]
  >   [ "echo" "8" foo:depends ]
  >   [ "echo" "9"    _:installed ]
  >   [ "echo" "10" foo:installed ]
  >   [ "echo" "11"   _:enable ]
  >   [ "echo" "12" foo:enable ]
  >   [ "echo" "13"   _:pinned ]
  >   [ "echo" "14" foo:pinned ]
  >   [ "echo" "15"   _:bin ]
  >   [ "echo" "16" foo:bin ]
  >   [ "echo" "17"   _:sbin ]
  >   [ "echo" "18" foo:sbin ]
  >   [ "echo" "19"   _:lib ]
  >   [ "echo" "20" foo:lib ]
  >   [ "echo" "21"   _:man ]
  >   [ "echo" "22" foo:man ]
  >   [ "echo" "23"   _:doc ]
  >   [ "echo" "24" foo:doc ]
  >   [ "echo" "25"   _:share ]
  >   [ "echo" "26" foo:share ]
  >   [ "echo" "27"   _:etc ]
  >   [ "echo" "28" foo:etc ]
  >   [ "echo" "29"   _:build ]
  >   [ "echo" "30" foo:build ]
  >   [ "echo" "31"   _:dev ]
  >   [ "echo" "32" foo:dev ]
  >   [ "echo" "33"   _:opamfile ]
  >   [ "echo" "34" foo:opamfile ]
  >   [ "echo" "35"     with-test ]
  >   [ "echo" "36"   _:with-test ]
  >   [ "echo" "37" foo:with-test ]
  >   [ "echo" "38"     with-doc ]
  >   [ "echo" "39"   _:with-doc ]
  >   [ "echo" "40" foo:with-doc ]
  >   [ "echo" "41"   _:with-dev-setup ]
  >   [ "echo" "42" foo:with-dev-setup ]
  > ]
  > EOF
  > mkpkg "foo" <<EOF
  > opam-version: "2.0"
  > EOF
  > solve testpkg
  Solution for dune.lock:
  foo.0.0.1
  testpkg.0.0.1
  
Inspecting the lockfile we can see how each opam package variable was translated into a
corresponding Dune version.

  $ cat dune.lock/testpkg.pkg
  (version 0.0.1)
  
  (build
   (progn
    (run echo 1 %{pkg-self:name})
    (run echo 2 %{pkg-self:name})
    (run echo 3 %{pkg:name:foo})
    (run echo 4 %{pkg-self:version})
    (run echo 5 %{pkg-self:version})
    (run echo 6 %{pkg:version:foo})
    (run echo 7 %{pkg-self:depends})
    (run echo 8 %{pkg:depends:foo})
    (run echo 9 %{pkg-self:installed})
    (run echo 10 %{pkg:installed:foo})
    (run echo 11 %{pkg-self:enable})
    (run echo 12 %{pkg:enable:foo})
    (run echo 13 %{pkg-self:pinned})
    (run echo 14 %{pkg:pinned:foo})
    (run echo 15 %{pkg-self:bin})
    (run echo 16 %{pkg:bin:foo})
    (run echo 17 %{pkg-self:sbin})
    (run echo 18 %{pkg:sbin:foo})
    (run echo 19 %{pkg-self:lib})
    (run echo 20 %{pkg:lib:foo})
    (run echo 21 %{pkg-self:man})
    (run echo 22 %{pkg:man:foo})
    (run echo 23 %{pkg-self:doc})
    (run echo 24 %{pkg:doc:foo})
    (run echo 25 %{pkg-self:share})
    (run echo 26 %{pkg:share:foo})
    (run echo 27 %{pkg-self:etc})
    (run echo 28 %{pkg:etc:foo})
    (run echo 29 %{pkg-self:build})
    (run echo 30 %{pkg:build:foo})
    (run echo 31 %{pkg-self:dev})
    (run echo 32 %{pkg:dev:foo})
    (run echo 33 %{pkg-self:opamfile})
    (run echo 34 %{pkg:opamfile:foo})
    (run echo 35 %{pkg-self:with-test})
    (run echo 36 %{pkg-self:with-test})
    (run echo 37 %{pkg:with-test:foo})
    (run echo 38 %{pkg-self:with-doc})
    (run echo 39 %{pkg-self:with-doc})
    (run echo 40 %{pkg:with-doc:foo})
    (run echo 41 %{pkg-self:with-dev-setup})
    (run echo 42 %{pkg:with-dev-setup:foo})))
  
  (deps foo)

The values here are not important, but Dune should be able to interpret the variables.

  $ build_pkg testpkg
  File "src/dune_rules/pkg_rules.ml", line 556, characters 17-23:
  File "src/dune_rules/pkg_rules.ml", line 556, characters 17-23: Assertion
  failed
  Raised at Dune_rules__Pkg_rules.Action_expander.Expander.expand_pform in file
    "src/dune_rules/pkg_rules.ml", line 556, characters 17-29
  Called from Dune_lang__String_with_vars.Make_expander.expand in file
    "src/dune_lang/string_with_vars.ml", line 271, characters 15-26
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  File "dune.lock/testpkg.pkg", line 5, characters 14-30:
  5 |   (run echo 1 %{pkg-self:name})
                    ^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 6, characters 14-30:
  6 |   (run echo 2 %{pkg-self:name})
                    ^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 8, characters 14-33:
  8 |   (run echo 4 %{pkg-self:version})
                    ^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 9, characters 14-33:
  9 |   (run echo 5 %{pkg-self:version})
                    ^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 11, characters 14-33:
  11 |   (run echo 7 %{pkg-self:depends})
                     ^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 13, characters 14-35:
  13 |   (run echo 9 %{pkg-self:installed})
                     ^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 15, characters 15-33:
  15 |   (run echo 11 %{pkg-self:enable})
                      ^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 17, characters 15-33:
  17 |   (run echo 13 %{pkg-self:pinned})
                      ^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 19, characters 15-30:
  19 |   (run echo 15 %{pkg-self:bin})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 21, characters 15-31:
  21 |   (run echo 17 %{pkg-self:sbin})
                      ^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 23, characters 15-30:
  23 |   (run echo 19 %{pkg-self:lib})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 25, characters 15-30:
  25 |   (run echo 21 %{pkg-self:man})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 27, characters 15-30:
  27 |   (run echo 23 %{pkg-self:doc})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 29, characters 15-32:
  29 |   (run echo 25 %{pkg-self:share})
                      ^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 31, characters 15-30:
  31 |   (run echo 27 %{pkg-self:etc})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 33, characters 15-32:
  33 |   (run echo 29 %{pkg-self:build})
                      ^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 35, characters 15-30:
  35 |   (run echo 31 %{pkg-self:dev})
                      ^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 37, characters 15-35:
  37 |   (run echo 33 %{pkg-self:opamfile})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 39, characters 15-36:
  39 |   (run echo 35 %{pkg-self:with-test})
                      ^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 40, characters 15-36:
  40 |   (run echo 36 %{pkg-self:with-test})
                      ^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 42, characters 15-35:
  42 |   (run echo 38 %{pkg-self:with-doc})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 43, characters 15-35:
  43 |   (run echo 39 %{pkg-self:with-doc})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  File "dune.lock/testpkg.pkg", line 45, characters 15-41:
  45 |   (run echo 41 %{pkg-self:with-dev-setup})
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{pkg-self:..}
  [1]
