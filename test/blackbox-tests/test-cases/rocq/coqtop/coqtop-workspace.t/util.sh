OUTPUT="$PWD/coqtop_test.tmp"

coqtop_test() {
  (true | (dune rocq top --display=verbose "$1" 1>$OUTPUT 2>&1)) || cat $OUTPUT
}
