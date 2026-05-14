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

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

A source copy that happens to match the source in one context must not suppress
invalidations for another context that has a stale copy. This simulates the case
where the file-watcher event for [data] is delivered after one copy has already
been updated, but before another copy has been invalidated.

  $ mkdir stale-copy-contexts
  $ cd stale-copy-contexts
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (context default)
  > (context
  >  (default
  >   (name other)))
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (deps data)
  >  (target result)
  >  (action (copy data result)))
  > EOF
  $ echo one > data
  $ start_dune
  $ build _build/default/result _build/other/result
  Success
  $ cat _build/default/result _build/other/result
  one
  one
  $ echo two > _build/default/data
  $ echo two > data
  $ build _build/other/result
  Success
  $ cat _build/other/result
  two
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  $ cd ..

The same suppression must not mask direct source readers, such as included dune
files, that are unrelated to the source-copy target.

  $ mkdir source-copy-and-read
  $ cd source-copy-and-read
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (deps data.inc)
  >  (target copy-result)
  >  (action (copy data.inc copy-result)))
  > (include data.inc)
  > EOF
  $ cat > data.inc <<EOF
  > (rule
  >  (target read-result)
  >  (action (write-file read-result "one")))
  > EOF
  $ start_dune
  $ build copy-result read-result
  Success
  $ cat _build/default/copy-result
  (rule
   (target read-result)
   (action (write-file read-result "one")))
  $ cat _build/default/read-result
  one
  $ cat > _build/default/data.inc <<EOF
  > (rule
  >  (target read-result)
  >  (action (write-file read-result "two")))
  > EOF
  $ cat > data.inc <<EOF
  > (rule
  >  (target read-result)
  >  (action (write-file read-result "two")))
  > EOF
  $ build read-result
  Success
  $ cat _build/default/read-result
  two
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  $ cd ..

Executable-bit changes are part of source-copy target digests. A copy that
already has the new executable bit in one context must not hide a stale copy in
another context.

  $ mkdir executable-source-copy
  $ cd executable-source-copy
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (context default)
  > (context
  >  (default
  >   (name other)))
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (deps script.sh)
  >  (target mode)
  >  (action
  >   (bash "dune_cmd stat permissions script.sh > mode")))
  > EOF
  $ printf '#!/bin/sh\n' > script.sh
  $ start_dune
  $ build _build/default/mode _build/other/mode
  Success
  $ cat _build/default/mode _build/other/mode
  644
  644
  $ chmod 655 _build/default/script.sh
  $ chmod 655 script.sh
  $ touch script.sh
  $ build _build/other/mode
  Success
  $ cat _build/other/mode
  655
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  $ cd ..

For [(source_tree ...)] dependencies, same-content edits should not rerun
dependent rules, while content edits should rerun them.

  $ mkdir source-tree-copy-cutoff
  $ cd source-tree-copy-cutoff
  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir tree
  $ printf one > tree/a
  $ cat > dune <<EOF
  > (rule
  >  (deps (source_tree tree) (sandbox none))
  >  (target result)
  >  (action
  >   (bash "cat tree/a >> ../../runs; printf '\n' >> ../../runs; cat tree/a > result")))
  > (alias
  >  (name idle))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ cat runs
  one
  $ printf one > tree/a
  $ build result
  Success
  $ cat runs
  one
  $ printf two > tree/a
  $ build result
  Success
  $ cat runs
  one
  two
  $ cat _build/default/result
  two
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  $ cd ..

Source copy trace events are available when the [source_copy] trace category is
enabled.

  $ mkdir source-copy-trace
  $ cd source-copy-trace
  $ echo '(lang dune 3.0)' > dune-project
  $ printf trace > source
  $ export DUNE_TRACE=source_copy
  $ dune build source
  $ dune trace cat | jq 'select(.cat == "source_copy" and .name == "copy") | {src: .args.src, dst: .args.dst, result: .args.result}'
  {
    "src": "source",
    "dst": "_build/default/source",
    "result": "copied"
  }
  $ dune build source
  $ dune trace cat | jq 'select(.cat == "source_copy" and .name == "copy") | {src: .args.src, dst: .args.dst, result: .args.result}'
  {
    "src": "source",
    "dst": "_build/default/source",
    "result": "already_up_to_date"
  }
  $ unset DUNE_TRACE
