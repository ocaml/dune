This test verifies the @pkg-install alias fetch and build the project dependencies
without building the project itself.

  $ . ./helpers.sh

Create a project using the fake library as a dependency:
  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

Ensure the alias is not available outside of the package manamgent context:
  $ dune build @pkg-install
  Error: The @pkg-install alias cannot be used without a lock dir
  -> required by alias pkg-install
  Hint: You might want to create the lock dir with 'dune pkg lock'
  [1]

Create a fake package which echoes information to stdout when build:
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
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

The alias call builds the `foo` dependency but not the project itself. It
displays the output of the fake package but not of the `bar.exe` executable:
  $ dune build @pkg-install
  Build package foo
  Install package foo

If we build the executable, it only shows the content of the executable as dune
already built the `foo` dependency when calling `@pkg-install`:

  $ dune build ./bar.exe
  let _ = 42
