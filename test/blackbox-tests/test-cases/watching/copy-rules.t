Test rules that copy source files in file-watching mode.

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

A missing directory initially has no rules. Creating a source directory must
invalidate that result, first for source copies and then for new local rules.

  $ build child/value
  Failure
  [1]
  $ mkdir child
  $ echo source > child/value
  $ build child/value
  Success
  $ cat _build/default/child/value
  source

  $ cat > child/dune <<EOF
  > (rule
  >  (target result)
  >  (deps value)
  >  (action (copy %{deps} %{target})))
  > EOF
  $ build child/result
  Success
  $ cat _build/default/child/result
  source

We're done.

  $ stop_dune_quiet

An executable-bit change must reach every build context, even when one copy
already has the new executable bit.

  $ mkdir executable-source-copy
  $ cd executable-source-copy
  $ make_dune_project 3.0
  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (context default)
  > (context (default (name other)))
  > EOF
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (deps script.sh)
  >  (target mode)
  >  (action
  >   (bash
  >    "if test -x script.sh; then echo executable; else echo plain; fi > mode")))
  > EOF
  $ printf '#!/bin/sh\n' > script.sh
  $ start_dune @idle
  $ build _build/default/mode _build/other/mode
  Success
  $ cat _build/default/mode _build/other/mode
  plain
  plain
  $ chmod +x _build/default/script.sh script.sh
  $ touch script.sh
  $ build _build/other/mode
  Success
  $ cat _build/other/mode
  executable
  $ stop_dune_quiet
  $ cd ..

A source_tree dependency must ignore same-content writes in watch mode, but
rerun its dependent rule after a real content change.

  $ mkdir source-tree-copy-cutoff
  $ cd source-tree-copy-cutoff
  $ make_dune_project 3.0
  $ runs="$PWD/../source-tree-runs"
  $ mkdir tree
  $ echo one > tree/a
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (deps (source_tree tree) (sandbox none))
  >  (target result)
  >  (action
  >   (progn
  >    (bash "echo run >> '$runs'")
  >    (copy tree/a result))))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ cat "$runs"
  run
  $ echo one > tree/a
  $ build result
  Success
  $ cat "$runs"
  run
  $ echo two > tree/a
  $ build result
  Success
  $ cat "$runs"
  run
  run
  $ cat _build/default/result
  two
  $ stop_dune_quiet
  $ cd ..

A source copy can become a regular file target and then a source copy again,
without changing or removing the source. Repeat the transition through a
regular directory target, which must invalidate old copies in its subtree.
Promotion is disabled so neither rule changes the source file.

  $ mkdir target-transitions
  $ cd target-transitions
  $ make_directory_targets_project 3.0
  $ mkdir tree
  $ printf 'source\n' > tree/value
  $ cat > .#source-rules <<EOF
  > (alias (name idle))
  > (rule
  >  (target result)
  >  (deps tree/value)
  >  (action (copy tree/value result)))
  > EOF
  $ cp .#source-rules dune
  $ start_dune --disable-promotion @idle
  $ build result
  Success
  $ if test -x _build/default/tree/value; then echo executable;
  > else echo plain; fi
  plain

The regular rule keeps the bytes identical but changes the executable bit.

  $ cat >> dune <<EOF
  > (subdir tree
  >  (rule
  >   (target value)
  >   (mode promote)
  >   (action
  >    (progn
  >     (write-file value "source\n")
  >     (run chmod +x value)))))
  > EOF
  $ build result
  Success
  $ if test -x _build/default/tree/value; then echo executable;
  > else echo plain; fi
  executable
  $ cmp tree/value _build/default/result

Switching back must restore the source's non-executable mode.

  $ cp .#source-rules dune
  $ build result
  Success
  $ if test -x _build/default/tree/value; then echo executable;
  > else echo plain; fi
  plain
  $ cmp tree/value _build/default/result

A directory rule can take over the parent of the same source copy.

  $ cat >> dune <<EOF
  > (rule
  >  (targets (dir tree))
  >  (mode promote)
  >  (action
  >   (bash "mkdir tree; echo source > tree/value; chmod +x tree/value")))
  > EOF
  $ build result
  Success
  $ if test -x _build/default/tree/value; then echo executable;
  > else echo plain; fi
  executable
  $ cmp tree/value _build/default/result
  $ cp .#source-rules dune
  $ build result
  Success
  $ if test -x _build/default/tree/value; then echo executable;
  > else echo plain; fi
  plain
  $ cmp tree/value _build/default/result

Later source changes must still invalidate the restored copy.

  $ printf 'edited\n' > tree/value
  $ build result
  Success
  $ cat _build/default/result
  edited
  $ stop_dune_quiet

Transitions must also work between separate Dune invocations.

  $ cat >> dune <<EOF
  > (subdir tree
  >  (rule
  >   (target value)
  >   (mode promote)
  >   (action
  >    (progn
  >     (write-file value "edited\n")
  >     (run chmod +x value)))))
  > EOF
  $ dune build result --disable-promotion
  $ test -x _build/default/tree/value
  $ cmp tree/value _build/default/result
  $ cp .#source-rules dune
  $ dune build result
  $ test ! -x _build/default/tree/value
  $ cmp tree/value _build/default/result
