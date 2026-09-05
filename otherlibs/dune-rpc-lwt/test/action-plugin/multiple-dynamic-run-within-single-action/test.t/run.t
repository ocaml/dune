Multiple dynamic invocations should retain their own working directories and
track all discovered dependencies. Currently both sequential and concurrent
composition are rejected while loading the rules.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.8)
  > (using action-plugin 0.1)
  > EOF
  $ cp ../bin/foo.exe .
  $ mkdir left right
  $ printf left > left/source
  $ printf right > right/source
  $ for dir in left right; do
  >   printf '%s\n' '(rule (target input) (deps source)' \
  >     ' (action (copy source input)))' > "$dir/dune"
  > done

A single invocation discovers its generated input from the changed directory.

  $ cat > dune <<'EOF'
  > (rule
  >  (target single)
  >  (action
  >   (with-stdout-to single (chdir left (dynamic-run ../foo.exe)))))
  > EOF
  $ dune build -j 4 single
  $ cat _build/default/single; echo
  left

Sequential invocations should concatenate both inputs and rebuild when a
read dependency changes. The rejection also blocks unrelated aliases in the
same dune file.

  $ cat > dune <<'EOF'
  > (rule
  >  (target sequential)
  >  (action
  >   (with-stdout-to sequential
  >    (progn
  >     (chdir left (dynamic-run ../foo.exe))
  >     (chdir right (dynamic-run ../foo.exe))))))
  > (rule (alias unused) (action (echo unused)))
  > EOF
  $ dune build @unused
  File "dune", lines 4-7, characters 2-125:
  4 |   (with-stdout-to sequential
  5 |    (progn
  6 |     (chdir left (dynamic-run ../foo.exe))
  7 |     (chdir right (dynamic-run ../foo.exe))))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
  $ dune build -j 4 sequential
  File "dune", lines 4-7, characters 2-125:
  4 |   (with-stdout-to sequential
  5 |    (progn
  6 |     (chdir left (dynamic-run ../foo.exe))
  7 |     (chdir right (dynamic-run ../foo.exe))))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
  $ test -f _build/default/sequential && { cat _build/default/sequential; echo; }
  [1]
  $ printf changed > right/source
  $ dune build -j 4 sequential
  File "dune", lines 4-7, characters 2-125:
  4 |   (with-stdout-to sequential
  5 |    (progn
  6 |     (chdir left (dynamic-run ../foo.exe))
  7 |     (chdir right (dynamic-run ../foo.exe))))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
  $ test -f _build/default/sequential && { cat _build/default/sequential; echo; }
  [1]

Test concurrent invocations separately so the sequential rule's rejection
does not mask their behavior.

  $ printf right > right/source
  $ cat > dune <<'EOF'
  > (rule
  >  (targets left-out right-out)
  >  (action
  >   (concurrent
  >    (with-stdout-to left-out (chdir left (dynamic-run ../foo.exe)))
  >    (with-stdout-to right-out (chdir right (dynamic-run ../foo.exe))))))
  > EOF
  $ dune build -j 4 left-out right-out
  File "dune", lines 4-6, characters 2-150:
  4 |   (concurrent
  5 |    (with-stdout-to left-out (chdir left (dynamic-run ../foo.exe)))
  6 |    (with-stdout-to right-out (chdir right (dynamic-run ../foo.exe))))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
  $ test -f _build/default/left-out && test -f _build/default/right-out &&
  > { cat _build/default/left-out _build/default/right-out; echo; }
  [1]
  $ printf changed > right/source
  $ dune build -j 4 left-out right-out
  File "dune", lines 4-6, characters 2-150:
  4 |   (concurrent
  5 |    (with-stdout-to left-out (chdir left (dynamic-run ../foo.exe)))
  6 |    (with-stdout-to right-out (chdir right (dynamic-run ../foo.exe))))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
  $ test -f _build/default/left-out && test -f _build/default/right-out &&
  > { cat _build/default/left-out _build/default/right-out; echo; }
  [1]
