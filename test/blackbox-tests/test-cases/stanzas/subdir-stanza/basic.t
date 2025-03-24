(subdir ..) allows us to interpret stanzas in a sub directory

  $ echo "(lang dune 2.5)" > dune-project
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo.txt (echo "bar")))
  > (subdir bar
  >  (rule (with-stdout-to foo.txt (echo "bar"))))
  > EOF
  $ dune build ./foo.txt ./bar/foo.txt
  $ cat _build/default/foo.txt
  bar


We can use paths such as foo/bar in (subdir ..)

  $ cat >dune <<EOF
  > (rule (with-stdout-to foo.txt (echo bar)))
  > (subdir bar/baz
  >  (subdir final
  >    (rule (with-stdout-to txt (echo final))))
  >  (rule (with-stdout-to foo.txt (echo bar))))
  > EOF
  $ dune build bar/baz/foo.txt bar/baz/final/txt
  $ cat _build/default/bar/baz/foo.txt
  bar
  $ cat _build/default/bar/baz/final/txt
  final

This is an error because we cannot specify data_only_dirs more than once per
dir.

  $ cat >dune <<EOF
  > (subdir bar (data_only_dirs foo))
  > EOF
  $ mkdir bar
  $ echo "(data_only_dirs foo)" > bar/dune
  $ dune build @all
  File "bar/dune", line 1, characters 16-19:
  1 | (data_only_dirs foo)
                      ^^^
  Error: This stanza stanza was already specified at:
  dune:1
  [1]

Overriding dune files in the sub directory is possible:

  $ mkdir override; cd override
  $ echo "(lang dune 2.5)" > dune-project
  $ cat >dune <<EOF
  > (data_only_dirs shadow)
  > (subdir shadow (rule (with-stdout-to bar (echo shadow))))
  > EOF
  $ mkdir shadow
  $ echo "does not work" > shadow/dune
  $ dune build ./shadow/bar
  $ cat _build/default/shadow/bar
  shadow
  $ cd ..

In conjunction with dune generated files:

  $ mkdir dune-syntax; cd dune-syntax
  $ echo "(lang dune 2.5)" > dune-project
  $ cat >dune <<EOF
  > (subdir sub (rule (with-stdout-to fromparent (echo parent))))
  > EOF
  $ mkdir sub
  $ cat >sub/dune <<EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|(rule (with-stdout-to bar (echo %{read:fromparent})))|};
  > EOF
  $ dune build ./sub/bar
  $ cat _build/default/sub/bar
  parent

subdir stanzas can also appear in included files

  $ mkdir -p include/subdir; cd include
  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > EOF
  $ cat >dune <<EOF
  > (include dune.inc)
  > EOF
  $ cat >dune.inc <<EOF
  > (subdir subdir
  >  (rule
  >   (action (with-stdout-to hello.txt (echo "Hello from subdir\n")))))
  > EOF
  $ dune build --root . subdir/hello.txt
  File "dune.inc", lines 1-3, characters 0-90:
  1 | (subdir subdir
  2 |  (rule
  3 |   (action (with-stdout-to hello.txt (echo "Hello from subdir\n")))))
  Error: Using a `subdir' stanza within an `include'd file is only available
  since version 2.7 of the dune language. Please update your dune-project file
  to have (lang dune 2.7).
  [1]
  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ dune build --root . subdir/hello.txt
  $ cat _build/default/subdir/hello.txt
  Hello from subdir

Include stanzas within subdir stanzas

  $ mkdir -p subdir-include/a; cd subdir-include
  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > EOF
  $ cat >dune <<EOF
  > (subdir a (include dune.inc))
  > EOF
  $ cat >a/dune.inc <<EOF
  > (rule (with-stdout-to hello.txt (echo Hello!)))
  > EOF
  $ dune build --root . a/hello.txt
  $ cat _build/default/a/hello.txt
  Hello!


  $ echo "(lang dune 2.5)" > dune-project
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo.txt (echo "bar")))
  > (subdir /absolute/path/to/bar
  >  (rule (with-stdout-to foo.txt (echo "bar"))))
  > EOF
  $ dune build ./foo.txt ./bar/foo.txt
  File "dune", line 2, characters 8-29:
  2 | (subdir /absolute/path/to/bar
              ^^^^^^^^^^^^^^^^^^^^^
  Error: invalid sub-directory path "/absolute/path/to/bar"
  Hint: sub-directory path must be relative
  [1]
