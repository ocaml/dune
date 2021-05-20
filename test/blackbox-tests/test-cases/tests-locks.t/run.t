These tests are run with locks. They should not end together (<> expected)
  $ dune build --root=. -j 2 --diff-command=diff @all-tests 2>&1 |
  > grep "^> *" | uniq -c | [ $(wc -l) -eq 1 ] && echo '=' || echo '<>'
  <>

These tests are run without locks. They should end together (= expected)
  $ dune build --root=. -j 2 --diff-command=diff @all-tests-nolocks 2>&1 |
  > grep "^> *" | uniq -c | [ $(wc -l) -eq 1 ] && echo '=' || echo '<>'
  =
