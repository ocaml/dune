A source-backed file can replace a build-directory target directory. When that
happens, stale workspace-local cache entries for the old directory's descendants
must be invalidated.

  $ make_directory_targets_project 3.0
  $ cat > dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (action (bash "mkdir foo; echo generated > foo/child")))
  > EOF
  $ dune build foo/child
  $ cat _build/default/foo/child
  generated
  $ echo > dune
  $ echo source > foo
  $ dune build foo
  $ cat _build/default/foo
  source
  $ rm foo
  $ cat > dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (action (bash "mkdir foo; echo generated > foo/child")))
  > EOF
  $ dune build foo/child
  $ cat _build/default/foo/child
  generated

Replacing a source with identical contents must not rerun dependent rules.

  $ mkdir identical-contents
  $ cd identical-contents
  $ make_dune_project 3.0
  $ cat > dune <<EOF
  > (rule
  >  (target result)
  >  (deps payload)
  >  (action
  >   (progn
  >    (run echo rebuilt)
  >    (copy payload result))))
  > EOF
  $ echo original > payload
  $ dune build result
  rebuilt
  $ cmp payload _build/default/result
  $ cp payload .#replacement
  $ mv .#replacement payload
  $ dune build result
  $ cmp payload _build/default/result
  $ echo changed > payload
  $ dune build result
  rebuilt
  $ cmp payload _build/default/result
  $ cd ..

A source-backed file must replace a generated symlink even when their
contents match, so unrelated builds cannot mutate the source snapshot.

  $ mkdir symlink-transition
  $ cd symlink-transition
  $ make_dune_project 3.0
  $ echo one > seed
  $ echo initial > trigger
  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (deps seed (sandbox none))
  >  (action (run ln -s seed x)))
  > (rule
  >  (target result)
  >  (deps x trigger)
  >  (action (copy x result)))
  > EOF
  $ dune build result
  $ cp seed x
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (target result)
  >  (deps x trigger)
  >  (action (copy x result)))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ if test -L _build/default/x; then echo symlink; else echo file; fi
  file
  $ echo two > seed
  $ build seed
  Success
  $ echo changed > trigger
  $ build result
  Success
  $ cat x _build/default/result
  one
  one
  $ stop_dune_quiet
  $ cd ..

A null build must reuse the source copy without comparing its contents.

  $ mkdir freshness
  $ cd freshness
  $ make_dune_project 3.0
  $ dd if=/dev/zero of=payload bs=1048576 count=1 2>/dev/null
  $ dune build payload --wait-for-filesystem-clock
  $ DUNE_TRACE=source_copy dune build payload
  $ dune trace cat | jq -s '
  >   [.[] | select(.cat == "source_copy" and .args.src == "payload"
  >                and .args.result != "cached")] | length'
  0

Same-size changes at comparison-buffer boundaries must update the build copy.

  $ printf x | dd of=payload bs=1 seek=65535 conv=notrunc 2>/dev/null
  $ dune build payload
  $ cmp payload _build/default/payload
  $ printf y | dd of=payload bs=1 seek=1048575 conv=notrunc 2>/dev/null
  $ dune build payload
  $ cmp payload _build/default/payload

Handle partial final buffers, size changes, and empty files too.

  $ printf tail >> payload
  $ dune build payload
  $ cmp payload _build/default/payload
  $ printf z | dd of=payload bs=1 seek=1048577 conv=notrunc 2>/dev/null
  $ dune build payload
  $ cmp payload _build/default/payload
  $ : > payload
  $ dune build payload
  $ cmp payload _build/default/payload

Executable-bit changes must invalidate the copy independently of its bytes.

  $ chmod +x payload
  $ dune build payload
  $ test -x _build/default/payload && echo executable
  executable
  $ chmod -x payload
  $ dune build payload
  $ if test -x _build/default/payload; then
  >   echo executable
  > else
  >   echo non-executable
  > fi
  non-executable
  $ cd ..

A source snapshot must also replace a generated hardlink to a mutable external
file, even when its initial contents match.

  $ echo one > external-seed
  $ external_seed="$PWD/external-seed"
  $ mkdir hardlink-transition
  $ cd hardlink-transition
  $ make_dune_project 3.0
  $ echo initial > trigger
  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (deps "$external_seed" (sandbox none))
  >  (action (run ln %{deps} x)))
  > (rule
  >  (target result)
  >  (deps x trigger)
  >  (action (copy x result)))
  > EOF
  $ dune build result
  $ cp "$external_seed" x
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (target result)
  >  (deps x trigger)
  >  (action (copy x result)))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ dune_cmd stat hardlinks _build/default/x
  1
  $ chmod u+w "$external_seed"
  $ echo two > "$external_seed"
  $ echo changed > trigger
  $ build result
  Success
  $ cat x _build/default/result
  one
  one
  $ stop_dune_quiet
  $ cd ..
