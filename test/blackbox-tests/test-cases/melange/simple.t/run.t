Compilation using melange
  $ dune build lib/.x.objs/melange/x.js
  $ node ./_build/default/lib/.x.objs/melange/x.js
  buy it

Rebuilding same project (js artifacts are tracked correctly)
  $ dune build lib/.x.objs/melange/x.js
  $ node ./_build/default/lib/.x.objs/melange/x.js
  buy it
