When there are explicit interfaces, modules must be rebuilt.
  $ echo 'let hello = "hello"' > lib_sub.ml

  $ dune runtest --display short
  hello

  $ mkdir 1 2
  $ cp _build/default/.main.eobjs/native/lib_sub.cmx ./1/
  $ cp _build/default/.main.eobjs/native/lib_sub.o ./1/

  $ echo 'let _x = 1' >> lib_sub.ml
  $ dune runtest --display short
  hello

  $ cp _build/default/.main.eobjs/native/lib_sub.cmx ./2/
  $ cp _build/default/.main.eobjs/native/lib_sub.o ./2/

  $ diffy() {
  > out1=$(mktemp)
  > out2=$(mktemp)
  > $1 1/lib_sub.o > $out1
  > $1 2/lib_sub.o > $out2
  > diff -u $out1 $out2
  > }

  $ diffy "readelf -a"
  $ diffy "readelf -s"
  $ diffy "objdump -s"
