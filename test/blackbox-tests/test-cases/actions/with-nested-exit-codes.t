Allows nested action modifiers under `with-accepted-exit-codes` in Dune 2.2.

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > (using action-plugin 0.1)
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias f)
  >  (action (with-accepted-exit-codes 
  >           1
  >           (with-stdout-to out.txt 
  >            (run dune_cmd exit-code 1)))))
  > EOF

  $ dune build --display=short --root . @f
      dune_cmd out.txt

  $ cat >> dune <<EOF
  > (rule
  >  (alias f2)
  >  (action (with-accepted-exit-codes
  >           1
  >           (with-stdin-from input
  >            (chdir .
  >             (run dune_cmd exit-code 1))))))
  > EOF

  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f2
      dune_cmd alias f2

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
  >               (run dune_cmd exit-code 1))))))))
  > EOF
  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f5
  File "dune", lines 28-36, characters 0-233:
  28 | (rule
  29 |  (alias f5)
  30 |  (action (with-accepted-exit-codes
  31 |           0
  32 |           (setenv VAR myvar
  33 |            (with-stdin-from input
  34 |             (chdir .
  35 |              (with-stdout-to out2.txt
  36 |               (run dune_cmd exit-code 1))))))))
      dune_cmd out2.txt (exit 1)
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
  >       (dynamic-run dune_cmd exit-code 1)))))))
  > EOF

  $ dune build --display=short --root . @g
  File "dune", lines 42-45, characters 3-106:
  42 |    (setenv VAR myvar
  43 |     (chdir .
  44 |      (with-stdout-to out.txt
  45 |       (dynamic-run dune_cmd exit-code 1)))))))
  Error: Only "run", "bash", "system", "chdir", "setenv", "ignore-<outputs>",
  "with-stdin-from", "with-<outputs>-to" and "no-infer" can be nested under
  "with-accepted-exit-codes"
  [1]
