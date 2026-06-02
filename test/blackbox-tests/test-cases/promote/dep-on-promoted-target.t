Depending on a promoted file works.

  $ make_dune_project 2.0

  $ write_promoted_target_rules() {
  > local mode="$1"
  > cat > dune <<EOF
  > (rule
  >   (mode ${mode})
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF
  > }

  $ write_promoted_target_rules promote

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

  $ write_promoted_target_rules standard

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

  $ write_promoted_target_rules fallback

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
