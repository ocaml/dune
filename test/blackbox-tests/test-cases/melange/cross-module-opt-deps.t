Melange emission must stage same-library interface artifacts that the emitter
loads implicitly.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using melange 1.0)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > (package (name foo))
  > EOF

  $ cat > dune <<'EOF'
  > (melange.emit
  >  (target dist)
  >  (modules)
  >  (emit_stdlib false)
  >  (libraries foo)
  >  (compile_flags :standard --mel-cross-module-opt))
  > 
  > (melange.emit
  >  (target dist-default-flags)
  >  (modules)
  >  (emit_stdlib false)
  >  (libraries foo))
  > EOF

  $ mkdir stdlib
  $ cat > stdlib/dune <<'EOF'
  > (library
  >  (public_name foo)
  >  (name stdlib)
  >  (modes melange)
  >  (preprocess (pps melange.ppx))
  >  (libraries melange.js)
  >  (stdlib
  >   (modules_before_stdlib CamlinternalFormatBasics)
  >   (internal_modules Camlinternal*))
  >  (melange.compile_flags --mel-no-cross-module-opt))
  > EOF

  $ cat > stdlib/stdlib.ml <<'EOF'
  > let char_code (_ : char) = 0
  > EOF
  $ cat > stdlib/stdlib.mli <<'EOF'
  > val char_code : char -> int
  > EOF

  $ cat > stdlib/char.ml <<'EOF'
  > let code c = char_code c
  > EOF
  $ cat > stdlib/char.mli <<'EOF'
  > val code : char -> int
  > EOF

  $ cat > stdlib/camlinternalFormatBasics.ml <<'EOF'
  > type t = unit
  > EOF
  $ cat > stdlib/camlinternalFormatBasics.mli <<'EOF'
  > type t = unit
  > EOF

The emitted `char.js` rule must keep the wrapped `Stdlib` module visible, even
though `Stdlib__Char` does not reach it through implementation-only deps.

  $ dune describe rules --display=quiet --profile=release dist/node_modules/foo/char.js > char-rules.sexp
  $ grep -c 'stdlib/.stdlib.objs/melange/stdlib.cmi' char-rules.sexp
  1
  $ grep -c 'stdlib/.stdlib.objs/melange/stdlib.cmj' char-rules.sexp
  1
  $ grep -c 'stdlib/.stdlib.objs/melange/camlinternalFormatBasics.cmi' char-rules.sexp
  1
  $ grep -c 'stdlib/.stdlib.objs/melange/stdlib__Char.cmi' char-rules.sexp
  1

The wrapped `Stdlib` module must also be visible when the emit stanza uses its
default compile flags.

  $ dune describe rules --display=quiet --profile=release dist-default-flags/node_modules/foo/char.js > char-default-rules.sexp
  $ grep -c 'stdlib/.stdlib.objs/melange/stdlib.cmi' char-default-rules.sexp
  0
  [1]
  $ grep -c 'stdlib/.stdlib.objs/melange/stdlib.cmj' char-default-rules.sexp
  0
  [1]

Build the default output explicitly in a sandbox so the regression does not
depend on the test suite's sandboxing preference. Without the wrapped root's
CMJ, Melange resolves `Stdlib` to its own runtime package instead of this custom
standard library.

  $ dune build --sandbox=symlink \
  > dist-default-flags/node_modules/foo/stdlib.js \
  > dist-default-flags/node_modules/foo/char.js
  $ cd _build/default/dist-default-flags/node_modules/foo
  $ grep -F 'require("melange/stdlib.js")' char.js
  const Stdlib = require("melange/stdlib.js");
  $ node -e 'console.log(require("./char.js").code("x"))' >/dev/null 2>&1
  [1]
