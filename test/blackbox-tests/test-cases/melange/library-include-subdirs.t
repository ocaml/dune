Test moving modules in a library with `(include_subdirs unqualified)`

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (libraries foo)
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx)))
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (modes melange))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let name = Bar.name
  > EOF

  $ mkdir lib/init
  $ cat > lib/init/bar.ml <<EOF
  > let name = "Zoe"
  > EOF

  $ dune build @mel

Melange shows the proper path to `bar.js`

  $ cat _build/default/output/lib/foo.js | grep bar.js
  const Foo__Bar = require("./init/bar.js");

  $ mv lib/init lib/end
  $ dune build @mel

The import in `foo.js` has been updated to the new bar.js target

  $ cat _build/default/output/lib/foo.js | grep bar.js
  const Foo__Bar = require("./end/bar.js");

The initial file is not there anymore

  $ test -f _build/default/output/lib/init/bar.js
  [1]

But the new one is

  $ test -f _build/default/output/lib/end/bar.js

Now try the same thing with `melange.emit`

  $ rm -rf lib
  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx)))
  > EOF

  $ mkdir init
  $ cat > init/bar.ml <<EOF
  > let name = "Zoe"
  > EOF
  $ cat > foo.ml <<EOF
  > let name = Bar.name
  > EOF

  $ dune build @mel

Melange shows the proper path to `bar.js`

  $ cat _build/default/output/foo.js | grep bar.js
  const Melange__Bar = require("./init/bar.js");

  $ mv init end
  $ dune build @mel

The import in `foo.js` has been updated to the new bar.js target

  $ cat _build/default/output/foo.js | grep bar.js
  const Melange__Bar = require("./end/bar.js");

The initial file is not there anymore

  $ test -f _build/default/output/init/bar.js
  [1]

But the new one is

  $ test -f _build/default/output/end/bar.js
