This test verifies the @pkg-source and @pkg-install aliases fetch and build the
project dependencies without building the project itself.

Create a project using the fake library as a dependency:
  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

Ensure the aliases are not available outside of the package management context:
  $ dune build @pkg-source
  Error: The @pkg-source alias cannot be used without a lock dir
  -> required by alias pkg-source
  Hint: You might want to create the lock dir with 'dune pkg lock'
  [1]
  $ dune build @pkg-install
  Error: The @pkg-install alias cannot be used without a lock dir
  -> required by alias pkg-install
  Hint: You might want to create the lock dir with 'dune pkg lock'
  [1]

Create fake packages which echo information to stdout when built:
  $ mkdir foo-source baz-source
  $ echo "Source for foo" > foo-source/source
  $ echo "Source for baz" > baz-source/source
  $ echo "Extra source for foo" > foo-extra-source
  $ tar cf foo-source.tar -C foo-source .
  $ tar cf baz-source.tar -C baz-source .
  $ make_lockdir
  $ make_lockpkg baz <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/baz-source.tar))
  > (build
  >  (run echo "Build package baz"))
  > (install
  >  (run echo "Install package baz"))
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (depends baz)
  > (source (copy $PWD/foo-source.tar))
  > (extra_sources
  >  (extra-source
  >   (fetch (url file://$PWD/foo-extra-source))))
  > (build
  >  (run echo "Build package foo"))
  > (install
  >  (run echo "Install package foo"))
  > EOF

Create a rule to show that this rule is not called with `@pkg-install` as `bar`
is not build when calling the alias. If called, it would output the content of
the `bar.ml` file:
  $ cat > dune << EOF
  > (executable
  >  (name bar))
  > (rule
  >  (target bar.ml)
  >  (action
  >   (progn
  >    (with-stdout-to %{target} (echo "let _ = 42"))
  >    (system "cat %{target}"))))
  > EOF

The @pkg-source alias fetches each package's primary and extra sources without
building the packages or the project:
  $ dune build @pkg-source
  $ cat _build/_private/default/.pkg/$(dune pkg print-digest baz)/source/source
  Source for baz
  $ cat _build/_private/default/.pkg/$(dune pkg print-digest foo)/source/source
  Source for foo
  $ cat _build/_private/default/.pkg/$(dune pkg print-digest foo)/extra_source/extra-source
  Extra source for foo

The @pkg-install alias call builds the dependencies but not the project itself.
It displays the output of the fake packages but not of the `bar.exe` executable:
  $ dune build @pkg-install
  Build package baz
  Install package baz
  Build package foo
  Install package foo

If we build the executable, it only shows the content of the executable as dune
already built the `foo` dependency when calling `@pkg-install`:

  $ dune build ./bar.exe
  let _ = 42
