Sometimes a user may misconfigure their editor and end up putting a tab
character before a command in a cram test instead of two spaces.

Dune would register this as a comment, leaving the user confused as to why
their command is not being run. The current behaviour is for a leading tab to
be treated as, and replaced by, two spaces during promotion.

  $ cat > mytest.t <<EOF
  > 	$ echo 'i ran without appearing';
  > 	> echo 'so did i'
  > 	$ echo 'me too'
  > 	me too
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ dune test mytest.t
  File "mytest.t", line 1, characters 0-0:
  Error: Files _build/default/mytest.t and _build/default/mytest.t.corrected
  differ.
  [1]

  $ dune promote
  Promoting _build/default/mytest.t.corrected to mytest.t.

The output of the cram test no longer has the tab characters and all the
commands are correctly run.

  $ cat mytest.t
    $ echo 'i ran without appearing';
    > echo 'so did i'
    i ran without appearing
    so did i
    $ echo 'me too'
    me too
