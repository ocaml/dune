Lending a process slot does not release its rule locks. A dependency requesting
the same lock currently deadlocks.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ cat > dune <<'EOF'
  > (rule
  >  (target shared-input)
  >  (locks shared)
  >  (action (write-file shared-input shared)))
  > (rule
  >  (alias shared)
  >  (locks shared)
  >  (action (dynamic-run ./foo.exe read shared-input)))
  > (rule
  >  (target ordered-input)
  >  (locks z)
  >  (action (write-file ordered-input ordered)))
  > (rule
  >  (alias ordered)
  >  (locks a)
  >  (action (dynamic-run ./foo.exe read ordered-input)))
  > (rule
  >  (target reversed-input)
  >  (locks a)
  >  (action (write-file reversed-input reversed)))
  > (rule
  >  (alias reversed)
  >  (locks z)
  >  (action (dynamic-run ./foo.exe read reversed-input)))
  > EOF
  $ timeout 3 dune build -j 1 @shared > shared.output 2>&1; echo $?
  124

Increasing lock acquisition order is safe across dynamic dependency requests.

  $ timeout 3 dune build -j 1 @ordered
  ordered

A reverse acquisition succeeds when uncontended, but can deadlock with another
rule holding the lower lock and waiting for the higher one.

  $ timeout 3 dune build -j 1 @reversed > reversed.output 2>&1; echo $?
  0
