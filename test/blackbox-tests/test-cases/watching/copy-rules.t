Test rules that copy source files in file-watching mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt))
  >   (target summary)
  >   (action (bash "cat %{deps} > %{target}")))
  > EOF
  $ echo a > a.txt

  $ start_dune summary

  $ dune_wait
  Success
  $ cat _build/default/summary
  a

Add [b.txt] manually. Dune notices this.

  $ echo b > b.txt
  $ dune_wait
  Success
  $ cat _build/default/summary
  a
  b

Now add [c.txt] via a new rule. Note that we do not request [c.txt] to be built,
only [summary], but Dune still builds it. Presumably, this isn't a bug but a
consequence of using a glob in this directory, which forces all *.txt rules.

  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt))
  >   (target summary)
  >   (action (bash "cat %{deps} > %{target}")))
  > (rule
  >   (target c.txt)
  >   (action (write-file %{target} c)))
  > EOF

  $ dune_wait
  Success
  $ cat _build/default/summary
  a
  b
  c

Now demonstrate a bug: Dune fails to notice that [d.txt] appears via promotion.

# CR-someday amokhov: Fix this test.

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (rule
  >   (target d.txt)
  >   (mode (promote (into ..)))
  >   (action (write-file %{target} d)))
  > EOF
  $ dune_wait
  Success
  $ cat _build/default/summary
  a
  b
  c

Note that neither [c.txt] nor [d.txt] are there. [c.txt] isn't (it's
not promoted), so that's normal. However, [d.txt] is promoted from
"subdir" into the current directory so it should be there but Dune
doesn't realise it:

  $ ls *.txt
  a.txt
  b.txt

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
