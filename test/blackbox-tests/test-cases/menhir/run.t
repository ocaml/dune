  $ $JBUILDER build -j1 src/test.exe --root . --debug-dependency-path
      ocamllex src/lexer1.ml
      ocamllex src/lexer2.ml
        menhir src/test_base.{ml,mli} (exit 1)
  (cd _build/default && C:\OCaml64\home\dimin\.opam\4.05.0+mingw64c\bin\menhir.exe --base test_base --explain tokens.mly parser.mly)
  Error: parser.mly: No such file or directory
  [1]
