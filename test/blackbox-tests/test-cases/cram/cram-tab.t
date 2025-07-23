Sometimes a user may misconfigure their editor and end up putting a tab
character before a command in a cram test instead of two spaces.

Dune will register this as a comment, leaving the user confused as to why their
command is not being run.

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

  $ dune promote

The output of the cram tests still includes all the tab characters and no
output has been updated.

  $ cat mytest.t
  	$ echo 'i ran without appearing';
  	> echo 'so did i'
  	$ echo 'me too'
  	me too
