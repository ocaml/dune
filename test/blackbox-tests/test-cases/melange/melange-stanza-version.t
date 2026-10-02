Test Melange extension stanza versioning

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

Version 0.1 is still supported in Dune 3.24

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > (package (name pkg))
  > EOF

  $ dune rules --root . --format=json app/.pkg.objs/melange/pkg__App.cmj |
  > jq_dune -r '.[] | ruleActionFlagValues("--bs-package-name")'
  pkg

Version 0.1 was deleted in Dune 3.25

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 0.1)
  > (package (name pkg))
  > EOF

  $ dune rules --root . --format=json app/.pkg.objs/melange/pkg__App.cmj
  File "dune-project", line 2, characters 15-18:
  2 | (using melange 0.1)
                     ^^^
  Error: Version 0.1 of the melange extension has been deleted in Dune 3.25.
  Please port this project to a newer version of the extension, such as 1.0.
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

Before Dune 3.26, Melange must be explicitly enabled.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (package (name pkg))
  > EOF
  $ cat > app/dune <<EOF
  > (library
  >  (public_name pkg)
  >  (modes :standard melange))
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (libraries pkg))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_endline Pkg.App.x
  > EOF

  $ dune build app/pkg.cma @@melange && node _build/default/dist/main.js
  File "dune", lines 1-3, characters 0-46:
  1 | (melange.emit
  2 |  (target dist)
  3 |  (libraries pkg))
  Error: 'melange.emit' is available only when melange is enabled in the
  dune-project or workspace file. You must enable it using (using melange 1.0)
  in the file.
  File "app/dune", line 3, characters 18-25:
  3 |  (modes :standard melange))
                        ^^^^^^^
  Error: 'melange' is available only when melange is enabled in the
  dune-project or workspace file. You must enable it using (using melange 1.0)
  in the file.
  [1]

Dune 3.26 enables Melange 1.0 without an explicit using declaration.

  $ cat > dune-project <<EOF
  > (lang dune 3.26)
  > (package (name pkg))
  > EOF
  $ dune build app/pkg.cma @@melange && node _build/default/dist/main.js
  hello
  $ dune rules --format=json app/.pkg.objs/melange/pkg__App.cmj > rules.json
  $ jq_dune -r '.[] | ruleActionFlagValues("--mel-package-name")' rules.json
  pkg

An explicit Melange 1.0 declaration remains valid.

  $ echo '(using melange 1.0)' >> dune-project
  $ dune build app/pkg.cma @@melange && node _build/default/dist/main.js
  hello

Automatically enabling Melange does not enable the ReScript dialect either.

  $ cat > rescript/dune-project <<EOF
  > (lang dune 3.26)
  > EOF
  $ dune build --root rescript
  Entering directory 'rescript'
  File "dune", line 4, characters 10-13:
  4 |  (modules app))
                ^^^
  Error: Module App doesn't exist.
  Leaving directory 'rescript'
  [1]
