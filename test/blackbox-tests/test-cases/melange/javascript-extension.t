Test (javascript_extension) field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

Can use extension with dots

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension bs.js))
  > EOF

  $ dune build @melange
  $ node _build/default/output/hello.bs.js
  hello

Errors out if extension starts with dot

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (module_system commonjs)
  >  (javascript_extension .bs.js))
  > EOF

  $ dune build @melange
  File "dune", line 5, characters 23-29:
  5 |  (javascript_extension .bs.js))
                             ^^^^^^
  Error: extension must not start with '.'
  [1]

Should apply the settig to libraries as well

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modules lib)
  >  (modes melange))
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (libraries lib)
  >  (entries foo)
  >  (module_system commonjs)
  >  (javascript_extension bs.js))
  > EOF

  $ cat >lib.ml <<EOF
  > let greeting = "Hello World"
  > EOF

  $ cat >foo.ml <<EOF
  > print_endline Lib.greeting
  > EOF

  $ dune build @melange
  $ ls _build/default/output/ | sort
  _build/default/output/foo.bs.js
  _build/default/output/lib.bs.js
  $ grep '.js' _build/default/output/foo.bs.js
  var Lib = require("./lib.js");
