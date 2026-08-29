These tests are run with locks. They should not end together (<> expected)
  $ dune build --root=. -j 2 --diff-command=diff @all-tests 2>&1 |
  > grep "^> *" | uniq -c | [ $(wc -l) -eq 1 ] && echo '=' || echo '<>'
  <>

These tests are run without locks. They should end together (= expected)
  $ dune build --root=. -j 2 --diff-command=diff @all-tests-nolocks 2>&1 |
  > grep "^> *" | uniq -c | [ $(wc -l) -eq 1 ] && echo '=' || echo '<>'
  =

Duplicate lock declarations are harmless; a rule acquires each lock only once.

  $ mkdir duplicate-lock
  $ cat >duplicate-lock/dune <<EOF
  > (rule
  >  (target done)
  >  (locks same same)
  >  (action (write-file done done)))
  > EOF
  $ if $timeout 2 dune build --root=. duplicate-lock/done >/dev/null 2>&1; then
  >   cat _build/default/duplicate-lock/done
  > else
  >   echo timed-out
  > fi
  done
