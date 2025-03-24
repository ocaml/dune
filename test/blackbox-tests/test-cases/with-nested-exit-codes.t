  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name exit)
  >  (modules exit))
  > (rule (with-stdout-to exit.ml (echo "let () = exit (int_of_string Sys.argv.(1))")))
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias f)
  >  (action (with-accepted-exit-codes 
  >           1
  >           (with-stdout-to out.txt 
  >            (run ./exit.exe 1)))))
  > EOF

  $ dune build --display=short --root . @f
        ocamlc .exit.eobjs/byte/dune__exe__Exit.{cmi,cmo,cmt}
      ocamlopt .exit.eobjs/native/dune__exe__Exit.{cmx,o}
      ocamlopt exit.exe
          exit out.txt

  $ cat >> dune <<EOF
  > (rule
  >  (alias f2)
  >  (action (with-accepted-exit-codes
  >           1
  >           (with-stdin-from input
  >            (chdir .
  >             (run ./exit.exe 1))))))
  > EOF

  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f2
          exit alias f2

  $ cat >> dune <<EOF
  > (rule
  >  (alias f3)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (chdir .
  >             (system "echo \$VAR"))))))
  > EOF

  $ dune build --display=short --root . @f3
            sh alias f3
  myvar

  $ cat >> dune <<EOF
  > (rule
  >  (alias f4)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (ignore-stdout
  >             (bash "echo \$VAR"))))))
  > EOF

  $ dune build --display=short --root . @f4
          bash alias f4

  $ cat >> dune <<EOF
  > (rule
  >  (alias f5)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (with-stdin-from input
  >             (chdir .
  >              (with-stdout-to out2.txt
  >               (run ./exit.exe 1))))))))
  > EOF
  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f5
  File "dune", lines 32-40, characters 0-225:
  32 | (rule
  33 |  (alias f5)
  34 |  (action (with-accepted-exit-codes
  35 |           0
  36 |           (setenv VAR myvar
  37 |            (with-stdin-from input
  38 |             (chdir .
  39 |              (with-stdout-to out2.txt
  40 |               (run ./exit.exe 1))))))))
          exit out2.txt (exit 1)
  [1]

  $ cat >> dune <<EOF
  > (rule
  >  (alias g)
  >  (action 
  >   (with-accepted-exit-codes 
  >    (not 0) 
  >    (setenv VAR myvar
  >     (chdir .
  >      (with-stdout-to out.txt
  >       (dynamic-run ./exit.exe 1)))))))
  > EOF

  $ dune build --display=short --root . @g
  File "dune", lines 46-49, characters 3-98:
  46 |    (setenv VAR myvar
  47 |     (chdir .
  48 |      (with-stdout-to out.txt
  49 |       (dynamic-run ./exit.exe 1)))))))
  Error: Only "run", "bash", "system", "chdir", "setenv", "ignore-<outputs>",
  "with-stdin-from", "with-<outputs>-to" and "no-infer" can be nested under
  "with-accepted-exit-codes"
  [1]
