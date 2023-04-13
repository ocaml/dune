#!/bin/sh

sed 's/$ (cd .*coqc/coqc/' $1 | \
sed 's/$ (cd .*coqdep/coqdep/' $1 | \
sed 's/$ (cd .*coqdoc/coqdoc/' $1 | \
sed 's/ -I/\n-I/g' | \
sed 's/ -nI/\n-nI/g' | \
sed 's/ -R/\n-R/g' | \
sed 's/ -w/\n-w/g' | \
sed 's/-I [^\n]*coq-core/-I coq-core/g' | \
sed 's/-nI [^\n]*coq-core/-I coq-core/' | \
sed 's/-I [^\n]*findlib/-I findlib/g' | \
sed 's/-I [^\n]*zarith/-I zarith/g' | \
sed 's/-I [^\n].*ocaml/-I ocaml/' | \
sed 's/-R [^\n]*coq/-R coq/g'
  