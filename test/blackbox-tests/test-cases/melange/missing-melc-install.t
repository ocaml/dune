Test multi-mode single context installed packages when melc is not available

Set up an environment that deliberately hides melc.

  $ export OCAMLLIB=$(ocamlc -where)
  $ mkdir _path
  $ for bin in dune ocamlc ocamldep ocamlopt ocamlobjinfo git gcc ar as ranlib \
  >   clang cc ld sh; do
  >   if command -v "$bin" > /dev/null; then
  >     ln -s "$(command -v "$bin")" _path/
  >   fi
  > done

Installed mixed-mode libraries are downgraded to their OCaml modes and remain
usable by installed consumers.

A bytecode and native library with Melange mode keeps both OCaml modes.

  $ mkdir package-byte-native
  $ cat > package-byte-native/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkg-byte-native))
  > (using melange 1.0)
  > EOF
  $ cat > package-byte-native/dune <<EOF
  > (library
  >  (public_name pkg-byte-native)
  >  (name mylib)
  >  (modules mylib)
  >  (modes :standard melange))
  > EOF
  $ cat > package-byte-native/mylib.ml <<EOF
  > let t = "hello from pkg-byte-native"
  > EOF

  $ mkdir consumer-byte-native
  $ cat > consumer-byte-native/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > consumer-byte-native/dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries pkg-byte-native))
  > EOF
  $ cat > consumer-byte-native/main.ml <<EOF
  > let () = print_endline Mylib.t
  > EOF

  $ (PATH=$PWD/_path dune build --root package-byte-native @install) && \
  > (PATH=$PWD/_path dune install --root package-byte-native --prefix "$PWD/prefix") && \
  > (PATH=$PWD/_path OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build \
  >    --root consumer-byte-native main.exe)

Melange-only libraries still require melc at the package boundary.

  $ mkdir package-melange-only
  $ cat > package-melange-only/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkg-melange-only))
  > (using melange 1.0)
  > EOF
  $ cat > package-melange-only/dune <<EOF
  > (library
  >  (public_name pkg-melange-only)
  >  (name mylib)
  >  (modules mylib)
  >  (modes melange))
  > EOF
  $ cat > package-melange-only/mylib.ml <<EOF
  > let t = "hello from pkg-melange-only"
  > EOF

  $ (PATH=$PWD/_path dune build --root package-melange-only @install)
  Entering directory 'package-melange-only'
  File ".mylib.objs/melange/_unknown_", line 1, characters 0-0:
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  Leaving directory 'package-melange-only'
  [1]

Package dependencies should follow only the modes that are installed. Package
`a` has separate OCaml and Melange requirements, so dropping the inactive
Melange dependency must not drop the OCaml dependency too.

  $ mkdir -p package-closure/a package-closure/ocaml-support \
  > package-closure/js-support
  $ cat >package-closure/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name a))
  > (package (name ocaml-support))
  > (package (name js-support))
  > EOF
  $ cat >package-closure/a/dune <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte melange)
  >  (libraries ocaml-support)
  >  (melange.libraries js-support))
  > EOF
  $ echo 'let value = 1' >package-closure/a/a.ml
  $ cat >package-closure/ocaml-support/dune <<'EOF'
  > (library (name ocaml_support) (public_name ocaml-support) (modes byte))
  > EOF
  $ echo 'let value = 2' >package-closure/ocaml-support/ocaml_support.ml
  $ cat >package-closure/js-support/dune <<'EOF'
  > (library (name js_support) (public_name js-support) (modes melange))
  > EOF
  $ echo 'let value = 3' >package-closure/js-support/js_support.ml
  $ cat >package-closure/dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package a))
  >  (action (write-file %{target} built)))
  > EOF

Ordinary installation of `a` succeeds without melc.

  $ PATH=$PWD/_path dune build --root package-closure a.install

The package dependency follows the active OCaml mode, not the inactive
Melange mode.

  $ PATH=$PWD/_path dune build --root package-closure result
  $ PATH=$PWD/_path dune rules --root package-closure --format=json result |
  > jq_dune 'rulesMatchingTarget("result") | {
  >   ocaml_dependency: ruleHasDepFile("lib/ocaml-support/dune-package"),
  >   melange_dependency: ruleHasDepFile("lib/js-support/dune-package")
  > }'
  {
    "ocaml_dependency": true,
    "melange_dependency": false
  }

With melc available, both dependency packages belong to the closure.

  $ dune build --root package-closure result
  $ dune rules --root package-closure --format=json result |
  > jq_dune 'rulesMatchingTarget("result") | {
  >   ocaml_dependency: ruleHasDepFile("lib/ocaml-support/dune-package"),
  >   melange_dependency: ruleHasDepFile("lib/js-support/dune-package")
  > }'
  {
    "ocaml_dependency": true,
    "melange_dependency": true
  }

Install both modes, then hide melc from a separate consumer. The installed
metadata describes existing artifacts; its Melange dependencies must still
be tracked without the compiler.

  $ dune build --root package-closure @install
  $ dune install --root package-closure --prefix "$PWD/closure-prefix"
  $ mkdir installed-closure
  $ echo '(lang dune 3.24)' >installed-closure/dune-project
  $ cp package-closure/dune installed-closure/dune
  $ PATH=$PWD/_path OCAMLPATH=$PWD/closure-prefix/lib dune build \
  > --root installed-closure result
  $ PATH=$PWD/_path OCAMLPATH=$PWD/closure-prefix/lib dune rules \
  > --root installed-closure --format=json result |
  > jq_dune 'rulesMatchingTarget("result") | {
  >   ocaml_dependency: ruleHasDepFile("lib/ocaml-support/dune-package"),
  >   melange_dependency: ruleHasDepFile("lib/js-support/dune-package")
  > }'
  {
    "ocaml_dependency": true,
    "melange_dependency": true
  }

Compiler availability is checked in each library's directory, not the rule's
root directory. A directory-local binary can enable the Melange mode even
when melc is absent from PATH.

  $ cat >package-closure/melc-wrapper <<EOF
  > #!$(command -v sh)
  > exec "$(command -v melc)" "\$@"
  > EOF
  $ chmod +x package-closure/melc-wrapper
  $ for dir in a js-support; do
  >   echo '(env (_ (binaries (../melc-wrapper as melc))))' \
  >   >>package-closure/$dir/dune
  > done
  $ PATH=$PWD/_path dune build --root package-closure result
  $ PATH=$PWD/_path dune rules --root package-closure --format=json result |
  > jq_dune 'rulesMatchingTarget("result") | {
  >   ocaml_dependency: ruleHasDepFile("lib/ocaml-support/dune-package"),
  >   melange_dependency: ruleHasDepFile("lib/js-support/dune-package")
  > }'
  {
    "ocaml_dependency": true,
    "melange_dependency": true
  }
