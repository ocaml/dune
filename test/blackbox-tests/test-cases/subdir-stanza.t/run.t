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
