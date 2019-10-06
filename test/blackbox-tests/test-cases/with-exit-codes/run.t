  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune <<EOF
  > (alias
  >  (name a)
  >  (action (with-exit-codes 0 (run false))))
  > EOF

  $ dune build --display=short --root . @a
         false alias a (exit 1)
  (cd _build/default && /usr/bin/false)
  [1]

  $ cat >> dune <<EOF
  > (alias
  >  (name b)
  >  (action (with-exit-codes (not 0) (run false))))
  > EOF

  $ dune build --display=short --root . @b
         false alias b

  $ cat >> dune <<EOF
  > (executable
  >  (name exit)
  >  (modules exit))
  > (rule (with-stdout-to exit.ml (echo "let () = exit (int_of_string Sys.argv.(1))")))
  > (alias
  >  (name c)
  >  (action (with-exit-codes (or 1 2 3) (run ./exit.exe 2))))
  > (alias
  >  (name d)
  >  (action (with-exit-codes (or 4 5 6) (run ./exit.exe 7))))
  > EOF

  $ dune build --display=short --root . @c
      ocamldep .exit.eobjs/exit.ml.d
        ocamlc .exit.eobjs/byte/dune__exe__Exit.{cmi,cmo,cmt}
      ocamlopt .exit.eobjs/native/dune__exe__Exit.{cmx,o}
      ocamlopt exit.exe
          exit alias c

  $ dune build --display=short --root . @d
          exit alias d (exit 7)
  (cd _build/default && ./exit.exe 7)
  [1]
