  $ $JBUILDER build -j1 --display short --root .
  File "jbuild", line 1, characters 0-155:
  Error: Public libraries may not have private dependencies.
  Private dependency "privatelib" encountered in public library:
  -> required by library "publiclib" in _build/default
      ocamldep publiclib.ml.d
  [1]
