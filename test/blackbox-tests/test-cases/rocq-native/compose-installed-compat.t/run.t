We make sure that Coq lang versions < 0.8 can still somewhat compose with
installed theories. The way this was done before was due to `-boot` not being
passed to Coq. This meant that we essentially have `-Q "" user-contrib` being
passed. In order to restore this behaviour for Coq lang < 0.8 we explicitly add
that flag. This PR makes sure that this is indeed the case.

We configure ROCQLIB to be lib/coq. Coq will search for user-contrib from here.
We also need to set up a fake Coq install.

  $ mkdir -p lib/coq
  $ export ROCQLIB=$PWD/lib/coq
  $ echo $ROCQLIB
  $TESTCASE_ROOT/lib/coq

  $ mkdir -p lib/coq/theories/Init/
  $ cat > lib/coq/theories/Init/Prelude.v << EOF
  > Inductive PreludeLoaded := Yes.
  > EOF

We need to manually compile the prelude.

  $ rocq compile -boot -noinit -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler no -R lib/coq/theories/ Corelib lib/coq/theories/Init/Prelude.v

We also setup some plugins

  $ mkdir -p lib/rocq-runtime/plugins

We setup an installed theory. Note that lib/coq/user-contrib doesn't exist yet,
so this also tests that it won't be a problem.

  $ dune build --root B @install
  $ dune install --root B --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo


Next we go into our Dune project and build it.
  $ dune build --root A
  Entering directory 'A'
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.
  Leaving directory 'A'

Now we check the flags that were passed to coqdep and coqc:

  $ dune trace cat | jq 'select(.cat == "process" and (.args.process_args.[0] | IN ("compile", "dep"))) | {name, args: (.args.process_args | map(sub(".*/coq-core"; "coq-core")))}'
  {
    "name": "rocq",
    "args": [
      "dep",
      "-boot",
      "-R",
      "$TESTCASE_ROOT/lib/coq/theories",
      "Corelib",
      "-Q",
      "$TESTCASE_ROOT/lib/coq/user-contrib/B",
      "B",
      "-R",
      ".",
      "A",
      "-dyndep",
      "opt",
      "-vos",
      "a.v"
    ]
  }
  {
    "name": "rocq",
    "args": [
      "compile",
      "-q",
      "-w",
      "-deprecated-native-compiler-option",
      "-native-output-dir",
      ".",
      "-native-compiler",
      "on",
      "-nI",
      "/home/runner/work/dune/dune/_opam/lib/rocq-runtime/kernel",
      "-nI",
      ".",
      "-boot",
      "-R",
      "$TESTCASE_ROOT/lib/coq/theories",
      "Corelib",
      "-Q",
      "$TESTCASE_ROOT/lib/coq/user-contrib/B",
      "B",
      "-R",
      ".",
      "A",
      "a.v"
    ]
  }
