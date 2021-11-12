Test target promotion in file-watching mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (mode promote)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF
  $ echo hi > original

  $ start_dune

  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/result
  hi
  hi

Now change the [original] and rebuild.

  $ echo bye > original
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now try deleting the promoted file.

  $ rm promoted
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now try replacing its content.

  $ echo hi > promoted
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now switch the mode to standard. Dune reports an error about multiple rules for
[_build/default/promoted], as expected (see the error at the end of the test).

  $ cat > dune <<EOF
  > (rule
  >   (mode standard)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF

  $ build result
  Failure

We use the hint and it starts to work.

  $ rm -f promoted
  $ build result
  Success
  $ cat promoted
  cat: promoted: No such file or directory
  [1]
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now use [fallback] to override the rule that generates [promoted].

  $ cat > dune <<EOF
  > (rule
  >   (mode fallback)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF

At first, we don't have the source, so the rule is used.

  $ build result
  Success
  $ cat promoted
  cat: promoted: No such file or directory
  [1]
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now we create the source file and it overrides the rule.

  $ echo hi > promoted
  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/promoted
  hi
  $ cat _build/default/result
  hi
  hi

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Error: Multiple rules generated for _build/default/promoted:
  - file present in source tree
  - dune:1
  Hint: rm -f promoted
  Had errors, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
