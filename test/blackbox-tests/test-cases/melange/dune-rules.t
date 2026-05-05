Test dune rules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ cat > main.ml <<EOF
  > Js.log "hello"
  > EOF

Calling dune rules with the 'all' alias works fine

  $ dune rules --root . --format=json @all |
  > jq -r 'include "dune"; .[] | ruleDepFilePaths | select(test("main\\.ml$|melange__Main\\.cmj$"))'
  _build/default/.melange_src/main.ml
  _build/default/main.ml
  _build/default/.output.mobjs/melange/melange__Main.cmj

Calling dune rules with the alias works fine

  $ dune rules --root . --format=json @melange |
  > jq -r 'include "dune"; .[] | ruleDepFilePaths | select(test("melange__Main\\.cmj$"))'
  _build/default/.output.mobjs/melange/melange__Main.cmj

Using output folder fails

  $ dune rules --root . --format=json output
  Error: Don't know how to build output
  [1]

Creating dir fixes the problem

  $ mkdir output

  $ dune rules --root . --format=json output |
  > jq -r 'include "dune"; rulesMatchingTarget("output/main.js") | ruleDepFilePaths | select(test("\\.cmj$"))'
  _build/default/.output.mobjs/melange/melange__Main.cmj
