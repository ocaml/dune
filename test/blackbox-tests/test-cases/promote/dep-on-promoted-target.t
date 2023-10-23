Depending on a promoted file works.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

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
  $ dune build result
  $ cat promoted
  hi
  $ cat _build/default/result
  hi
  hi

Now change the [original] and rebuild.

  $ echo bye > original
  $ dune build result
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now switch the mode to standard. Dune reports an error about multiple rules for
[_build/default/promoted], as expected.

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

  $ dune build result
  Error: Multiple rules generated for _build/default/promoted:
  - dune:1
  - file present in source tree
  Hint: rm -f promoted
  [1]

We use the hint and it starts to work.

  $ rm -f promoted
  $ dune build result
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

  $ dune build result
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
  $ dune build result
  $ cat promoted
  hi
  $ cat _build/default/promoted
  hi
  $ cat _build/default/result
  hi
  hi
