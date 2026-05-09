Test promotion of source-backed artifacts in file-watching mode.

  $ export DUNE_TRACE="cache"

Promoted files should be available through their source-copy targets in _build.

  $ mkdir promoted-source-copy
  $ cd promoted-source-copy
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original)
  >  (target promoted)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (deps promoted)
  >  (target result)
  >  (action (system "cat promoted promoted > result")))
  > EOF
  $ echo hi > original
  $ start_dune
  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/promoted
  hi
  $ cat _build/default/result
  hi
  hi
  $ echo bye > original
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye
  $ stop_dune > /dev/null
  $ cd ..

Promoting a source file must not suppress events that are needed to refresh a
source-copy target for the same source path.

  $ mkdir promote-and-source-copy
  $ cd promote-and-source-copy
  $ echo '(lang dune 3.23)' > dune-project
  $ mkdir sub
  $ printf old > data
  $ runs="$PWD/runs"
  $ cat > dune <<EOF
  > (rule
  >  (deps data)
  >  (target result)
  >  (action
  >   (bash "cat data > result; printf 'copy:%s\n' \"\$(cat data)\" >> $runs")))
  > (alias
  >  (name idle))
  > EOF
  $ cat > sub/dune <<EOF
  > (rule
  >  (mode (promote (into ..)))
  >  (target data)
  >  (action (bash "printf new > data; printf 'promote\n' >> $runs")))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/default/result
  old
  $ build sub/data
  Success
  $ with_timeout dune rpc flush-file-watcher --wait
  $ cat data
  new
  $ build result
  Success
  $ cat _build/default/result
  old
  $ cat runs
  copy:old
  copy:old
  promote
  $ stop_dune > /dev/null
  $ cd ..

File-system events caused by promotion should not run the promotion rule twice.
Same-content events for promoted source files should not rerun dependent rules,
but real content changes should still be restored by promotion.

  $ mkdir promotion-same-content
  $ cd promotion-same-content
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original (sandbox none))
  >  (target promoted)
  >  (action
  >   (bash "cp original promoted; printf 'promote\n' >> ../runs")))
  > (rule
  >  (deps promoted (sandbox none))
  >  (target result)
  >  (action
  >   (bash "cat promoted > result; printf 'result\n' >> ../runs")))
  > (alias
  >  (name idle))
  > EOF
  $ echo one > original
  $ echo stale > promoted
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ touch promoted
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ echo changed > promoted
  $ build result
  Success
  $ cat promoted
  one
  $ cat _build/runs
  promote
  result
  $ stop_dune > /dev/null
  $ cd ..

Promotion up-to-date checks must include the executable bit, not just bytes.

  $ mkdir promotion-executable-bit
  $ cd promotion-executable-bit
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original (sandbox none))
  >  (target promoted)
  >  (action
  >   (bash "cp original promoted; chmod 655 promoted; printf 'promote\n' >> ../runs")))
  > (rule
  >  (deps promoted (sandbox none))
  >  (target result)
  >  (action
  >   (bash "dune_cmd stat permissions promoted > result; cat result >> ../runs")))
  > (alias
  >  (name idle))
  > EOF
  $ echo one > original
  $ echo one > promoted
  $ chmod 644 promoted
  $ start_dune @idle
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ chmod 644 promoted
  $ touch promoted
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ stop_dune > /dev/null
