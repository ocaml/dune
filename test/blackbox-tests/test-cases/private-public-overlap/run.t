  $ $JBUILDER build -j1 --display short --root .
      ocamldep publiclib.ml.d
  Error: Public libraries may not have private dependencies.
  Private dependency "privatelib" encountered in public library:
  - "publiclib" in .
  [1]
