`dune runtest --force` should re-run mdx test actions, even when the source
hasn't changed.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using mdx 0.2)
  > EOF

  $ cat > dune <<EOF
  > (mdx
  >  (files doc.md))
  > EOF

The markdown contains a shell block which writes to a `side-effect` local file.
Every time mdx runs the block it appends a line to the file in the test root.
Counting those lines tells us how many times the action actually executed.

  $ cat > doc.md <<EOF
  > \`\`\`sh
  > \$ echo run >> $PWD/side-effect
  > \`\`\`
  > EOF

First run executes the action once:

  $ dune runtest
  $ cat side-effect
  run

A plain re-run does nothing (nothing changed):

  $ dune runtest
  $ cat side-effect
  run

Forcing a re-run executes the action again, adding a second "run":

  $ dune runtest --force
  $ cat side-effect
  run
  run
