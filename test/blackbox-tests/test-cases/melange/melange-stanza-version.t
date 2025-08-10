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
  $ dune rules app/.pkg.objs/melange/pkg__App.cmj
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
  $ dune rules app/.pkg.objs/melange/pkg__App.cmj |
  > grep -e "--mel-package-name" --after-context=1
      --mel-package-name
      pkg


Using `(module_system es6)` is deprecated in `(using melange 1.0)`

  $ cat > app/dune <<EOF
  > (melange.emit (target dist) (module_systems es6))
  > EOF
  $ dune rules @app/melange > /dev/null
  File "app/dune", line 1, characters 44-47:
  1 | (melange.emit (target dist) (module_systems es6))
                                                  ^^^
  Warning: 'es6' was deprecated in version 1.0 of the Melange extension. Use
  `esm' instead.

