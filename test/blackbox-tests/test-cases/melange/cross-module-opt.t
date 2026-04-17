When `--mel-cross-module-opt` is enabled, Melange emission for an ordinary
library should stage same-library dependencies discovered from the emitted
module's interface.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using melange 1.0)
  > (package (name repro))
  > EOF

  $ cat > dune <<'EOF'
  > (melange.emit
  >  (target dist_xopt)
  >  (modules)
  >  (module_systems (commonjs js))
  >  (libraries repro)
  >  (compile_flags :standard --mel-cross-module-opt))
  > 
  > (melange.emit
  >  (target dist_no_xopt)
  >  (modules)
  >  (module_systems (commonjs js))
  >  (libraries repro)
  >  (compile_flags :standard --mel-no-cross-module-opt))
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name repro)
  >  (public_name repro)
  >  (wrapped false)
  >  (modes melange)
  >  (libraries melange.js)
  >  (melange.compile_flags -mel-cross-module-opt))
  > EOF

  $ cat > lib/entry.mli <<'EOF'
  > val f : Dep.t -> Dep.t
  > EOF

  $ cat > lib/entry.ml <<'EOF'
  > let f x = x
  > EOF

  $ cat > lib/dep.mli <<'EOF'
  > type t = Leaf.t
  > EOF

  $ cat > lib/dep.ml <<'EOF'
  > type t = Leaf.t
  > EOF

  $ cat > lib/leaf.mli <<'EOF'
  > type t = string
  > EOF

  $ cat > lib/leaf.ml <<'EOF'
  > type t = string
  > EOF

With xopt enabled, the `entry.js` emission rule must stage the interface
closure and the same-library implementation closure it reaches.

  $ dune describe rules --root . --format=json --display=quiet --profile=release dist_xopt/node_modules/repro/entry.js > xopt-rules.json
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/dep\\.cmi$")) ] | length' xopt-rules.json
  1
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/dep\\.cmj$")) ] | length' xopt-rules.json
  1
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/leaf\\.cmi$")) ] | length' xopt-rules.json
  1
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/leaf\\.cmj$")) ] | length' xopt-rules.json
  1

Without xopt, the same emitted module should not stage those extra same-library
dependencies.

  $ dune describe rules --root . --format=json --display=quiet --profile=release dist_no_xopt/node_modules/repro/entry.js > no-xopt-rules.json
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/dep\\.cmi$")) ] | length' no-xopt-rules.json
  0
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/dep\\.cmj$")) ] | length' no-xopt-rules.json
  0
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/leaf\\.cmi$")) ] | length' no-xopt-rules.json
  0
  $ jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("lib/\\.repro\\.objs/melange/leaf\\.cmj$")) ] | length' no-xopt-rules.json
  0
