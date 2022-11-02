Test rules that copy source files in file-watching mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt))
  >   (target summary)
  >   (action (system "cat %{deps} > %{target}")))
  > EOF
  $ echo a > a.txt

  $ start_dune

  $ build summary
  Success
  $ cat _build/default/summary
  a

Add [b.txt] manually. Dune notices this.

  $ echo b > b.txt
  $ build summary
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
  >   (action (system "cat %{deps} > %{target}")))
  > (rule
  >   (target c.txt)
  >   (action (write-file %{target} "c\n")))
  > EOF

  $ build summary
  Success
  $ cat _build/default/summary
  a
  b
  c

Dune notices that [d.txt] appears via promotion.

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (rule
  >   (target d.txt)
  >   (mode (promote (into ..)))
  >   (action (write-file %{target} "d\n")))
  > EOF
  $ build summary subdir/d.txt
  Success
  $ cat _build/default/summary
  a
  b
  c
  d

Note that [d.txt] is here but [c.txt] isn't (it's not promoted).

  $ ls *.txt
  a.txt
  b.txt
  d.txt

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
