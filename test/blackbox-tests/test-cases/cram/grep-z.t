Test of the cram test framework itself

Grep should work

  $ cat > multi-line.opam <<EOF
  > x-before: "yes"
  > depends: [
  >   "a"
  > ]
  x-after: "true"
  EOF
  $ cat multi-line.opam

Grep should not fail
  
  $ grep -Pzo "(?s)depends:.*?]\n" < multi-line.opam
  depends: [
    "a"
  ]
