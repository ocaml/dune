  $ $JBUILDER build --root . -j1 --display quiet b/b.cma
        ocamlc b/.b.objs/plop.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /home/dim/.opam/4.05.0/bin/ocamlc.opt -w -40 -g -bin-annot -I a/.a.objs -no-alias-deps -I b/.b.objs -o b/.b.objs/plop.cmo -c -impl b/plop.ml)
  File "b/plop.ml", line 1, characters 23-28:
  Error: This expression has type int but an expression was expected of type
           string
  [1]
