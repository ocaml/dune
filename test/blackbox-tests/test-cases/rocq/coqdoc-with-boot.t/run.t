Testing coqdoc when composed with a boot library

  $ dune build A/A.html

  $ ls _build/default/A
  A.html
  a.glob
  a.v
  a.vo
  a.vok
  a.vos

Dune should be passing '--coqlib' to coqdoc, but it doesn't. This is a bug.

  $ dune trace cat | jq -c '.[] | select(.cat == "process" and .args.process_args.[0] == "doc") | .args.process_args'
  ["doc","-R","../Coq","Corelib","-R",".","A","--toc","--html","-d","A.html","a.v"]
