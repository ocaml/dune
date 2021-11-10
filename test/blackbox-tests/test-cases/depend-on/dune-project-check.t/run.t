# Test dependency check in dune-project

  $ mkdir a b prefix

  $ cat >a/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name story))
  > (package (name cat) (depends story))
  > EOF

  $ cat >a/dune <<EOF
  > (library (public_name cat) (modules cat) (libraries story))
  > (library (public_name story) (modules story))
  > EOF

  $ cat >a/story.ml <<EOF
  > let say = Printf.sprintf "I'm saying %s"
  > EOF

  $ cat >a/cat.ml <<EOF
  > let story = Story.say "miaou"
  > EOF

  $ dune build --root a
  Entering directory 'a'

  $ dune install --root a --prefix $(pwd)/prefix
  Entering directory 'a'
  Installing $TESTCASE_ROOT/prefix/lib/cat/META
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.a
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cma
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cmi
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cmt
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cmx
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.ml
  Installing $TESTCASE_ROOT/prefix/lib/cat/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/cat/cat.cmxs
  Installing $TESTCASE_ROOT/prefix/lib/story/META
  Installing $TESTCASE_ROOT/prefix/lib/story/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/story/story.a
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cma
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cmi
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cmt
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cmx
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/story/story.ml
  Installing $TESTCASE_ROOT/prefix/lib/story/story.cmxs

  $ cat >b/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name loud_cat))
  > EOF

  $ cat >b/dune <<EOF
  > (library (public_name loud_cat) (modules loud_cat) (libraries cat))
  > EOF

  $ cat >b/loud_cat.ml <<EOF
  > let story = String.uppercase_ascii Cat.story
  > EOF

# The missing dependency in `loud_cat` dune-project is not detected
  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @install
  Entering directory 'b'

# Once directory `a` is local, it is detected
  $ rm -rf a/_build

  $ cp -a a b/a

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @install
  Entering directory 'b'
  Error: Package loud_cat is missing the following package dependencies
  - cat
  - story
  -> required by _build/default/loud_cat.install
  -> required by alias install
  [1]

# But all the transitive dependencies needs to be added to fix the check
  $ cat >b/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name loud_cat) (depends cat))
  > EOF

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @install
  Entering directory 'b'
  Error: Package loud_cat is missing the following package dependencies
  - story
  -> required by _build/default/loud_cat.install
  -> required by alias install
  [1]


# With all the transitive dependencies
  $ cat >b/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name loud_cat) (depends cat story))
  > EOF

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @install
  Entering directory 'b'
