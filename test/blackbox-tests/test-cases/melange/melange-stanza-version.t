Test Melange extension stanza versioning

Suggest newer language versions when none of the extension versions are
available in the selected dune language.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using melange 1.0)
  > EOF
  $ dune build
  File "dune-project", line 2, characters 15-18:
  2 | (using melange 1.0)
                     ^^^
  Error: Version 1.0 of the Melange extension is not supported until version
  3.20 of the dune language.
  There are no supported versions of this extension in version 3.0 of the dune
  language.
  
  Supported versions in newer versions of the dune language:
  - 0.1
  - 1.0
  Hint: The first version of this extension is 0.1 and is available since (lang
  dune 3.8).
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 1.0)
  > (package (name pkg))
  > EOF

  $ mkdir app
  $ cat > app/dune <<EOF
  > (library
  >  (public_name pkg)
  >  (modes melange))
  > EOF
  $ cat > app/app.ml <<EOF
  > let x = "hello"
  > EOF

Only supported after Dune 3.20
  $ dune rules --root . --format=json app/.pkg.objs/melange/pkg__App.cmj
  File "dune-project", line 2, characters 15-18:
  2 | (using melange 1.0)
                     ^^^
  Error: Version 1.0 of the Melange extension is not supported until version
  3.20 of the dune language.
  Supported versions of this extension in version 3.8 of the dune language:
  - 0.1
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > (package (name pkg))
  > EOF

Cmj rules should include --mel-package-output
  $ dune rules --root . --format=json app/.pkg.objs/melange/pkg__App.cmj |
  > jq_dune -r '.[] | ruleActionFlagValues("--mel-package-name")'
  pkg


Using `(module_system es6)` is deprecated in `(using melange 1.0)`

  $ cat > app/dune <<EOF
  > (melange.emit (target dist) (module_systems es6))
  > EOF
  $ dune rules --root . --format=json @app/melange > /dev/null
  File "app/dune", line 1, characters 44-47:
  1 | (melange.emit (target dist) (module_systems es6))
                                                  ^^^
  Warning: 'es6' was deprecated in version 1.0 of the Melange extension. Use
  `esm' instead.

Melange 1.0 does not implicitly enable the ReScript dialect

  $ mkdir rescript
  $ cat > rescript/dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > EOF
  $ cat > rescript/dune <<EOF
  > (library
  >  (name app)
  >  (modes melange)
  >  (modules app))
  > EOF
  $ cat > rescript/app.res <<EOF
  > let x = "hello"
  > EOF
  $ dune build --root rescript
  Entering directory 'rescript'
  File "dune", line 4, characters 10-13:
  4 |  (modules app))
                ^^^
  Error: Module App doesn't exist.
  Leaving directory 'rescript'
  [1]
