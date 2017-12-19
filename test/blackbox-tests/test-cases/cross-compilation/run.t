  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf $JBUILDER build --root . -j1 -x foo file
  $ cat _build/default.foo/file
