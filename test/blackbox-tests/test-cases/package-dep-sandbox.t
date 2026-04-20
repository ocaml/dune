Depending on (package foo) should make foo's install artifacts available
when the rule is sandboxed. Currently this is broken: the sandbox does not
include the package install directory.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir foo

  $ cat  >foo/dune <<EOF
  > (executable
  >  (name foo_bin)
  >  (public_name foo-bin)
  >  (package foo))
  > 
  > (install
  >  (section share)
  >  (package foo)
  >  (files (data.txt as data.txt)))
  > EOF

  $ cat >foo/foo_bin.ml <<EOF
  > let () = print_endline "I am foo"
  > EOF

  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias check-foo)
  >  (deps (package foo))
  >  (action
  >   (bash
  >    "\| echo bin:;
  >    "\| ls ../install/default/bin/ 2>&1;
  >    "\| echo share:;
  >    "\| ls ../install/default/share/foo/ 2>&1
  >   )))
  > EOF

With sandboxing (default for lang dune 3.23), install dirs are empty:

  $ dune build @check-foo 2>&1
  bin:
  share:

Without sandboxing (lang dune 3.22), the artifacts are visible:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias check-foo-nosandbox)
  >  (deps (package foo))
  >  (action
  >   (bash
  >    "\| echo bin:;
  >    "\| ls ../install/default/bin/ 2>&1;
  >    "\| echo share:;
  >    "\| ls ../install/default/share/foo/ 2>&1
  >   )))
  > EOF

  $ dune clean 2>&1

  $ dune build @check-foo-nosandbox 2>&1
  bin:
  foo-bin
  share:
  data.txt
