----------------------------------------------------------------------------------
Testsuite for (bisect_ppx) field for libraries/executables.

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > (using bisect_ppx 1.0)
  > EOF


  $ cat >dune-workspace.dev <<EOF
  > (lang dune 2.6)
  > (context (default (bisect_enabled true)))
  > EOF

  $ echo "let run () = print_endline \"Hello, world!\"" > hello.ml
  $ echo "let () = Hello.run ()" > test.ml

----------------------------------------------------------------------------------
* (preprocess (pps bisect_ppx)) produces the correct coverage files.

  $ cat >dune <<EOF
  > (executable
  >  (name test)
  >  (preprocess (per_module
  >               ((pps bisect_ppx) hello))))
  > EOF

  $ dune exec ./test.exe
  Hello, world!

  $ dune exec bisect-ppx-report -- html
  Info: found coverage files in './'

  $ find . -name "hello.ml.html"
  ./_coverage/hello.ml.html

  $ find . -name "test.ml.html"
  $ rm *.coverage
  $ rm -rf _coverage

----------------------------------------------------------------------------------
Test that bisect_ppx is enabled and produces *.coverage file for libraries.

  $ cat >dune <<EOF
  > (library
  >  (name hello)
  >  (modules hello)
  >  (bisect_ppx))
  > 
  > (executable
  >  (name test)
  >  (modules test)
  >  (libraries hello))
  > EOF

  $ dune exec ./test.exe
  Hello, world!

  $ dune exec bisect-ppx-report -- html
  Error: no coverage files given on command line or found
  [1]

  $ dune exec ./test.exe --workspace dune-workspace.dev
  Hello, world!

  $ dune exec bisect-ppx-report -- html
  Info: found coverage files in './'

  $ find . -name "hello.ml.html"
  ./_coverage/hello.ml.html
  $ rm *.coverage
  $ rm -rf _coverage

Test that bisect_ppx is enabled and produces *.coverage file for executables.
  $ cat >dune <<EOF
  > (library
  >  (name hello)
  >  (modules hello))
  > 
  > (executable
  >  (name test)
  >  (modules test)
  >  (bisect_ppx)
  >  (libraries hello))
  > EOF

  $ dune exec ./test.exe
  Hello, world!

  $ dune exec bisect-ppx-report -- html
  Error: no coverage files given on command line or found
  [1]

  $ dune exec ./test.exe --workspace dune-workspace.dev
  Hello, world!

  $ dune exec bisect-ppx-report -- html
  Info: found coverage files in './'

  $ find . -name "test.ml.html"
  ./_coverage/test.ml.html
  $ rm *.coverage
  $ rm -rf _coverage
