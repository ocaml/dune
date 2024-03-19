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
  let Foo__Bar = require("./init/bar.js");

  $ mv lib/init lib/end

  $ dune build @mel

The import in `foo.js` still shows the initial path to `bar.js`, but the file is not there anymore

  $ cat _build/default/output/lib/foo.js | grep bar.js
  let Foo__Bar = require("./init/bar.js");

  $ test -f _build/default/output/lib/init/bar.js
  [1]

  $ test -f _build/default/output/lib/end/bar.js

After removal of the js artifact, the path in `bar.js` import is correct

  $ rm _build/default/output/lib/foo.js

  $ dune build @mel

  $ cat _build/default/output/lib/foo.js | grep bar.js
  let Foo__Bar = require("./end/bar.js");
