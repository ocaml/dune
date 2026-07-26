This test exercises concurrent installation of entries from one install file.
Independent destinations may be copied in parallel, while conflicts retain
their sequential behavior and console output prints in deterministic order.

  $ make_dune_project_with_package 3.25 foo
  $ cat >dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cat >foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ dune build @install
  $ chmod u+w _build/default/foo.install

Conflicting destinations retain their sequential behavior.

Exact duplicates are installed in order:

  $ printf 'first\n' >first
  $ printf 'second\n' >second
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "first" {"race"}
  >   "second" {"race"}
  > ]
  > EOF
  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/race
  Deleting prefix/lib/foo/race
  Installing prefix/lib/foo/race
  $ cat prefix/lib/foo/race
  second

Ancestor/descendant destinations are also processed sequentially. The first
entry installs `node` as a file, so the second cannot create `node/child` and
fails deterministically:

  $ rm -rf prefix
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "first" {"node"}
  >   "second" {"node/child"}
  > ]
  > EOF
  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/node
  Error: Please delete file prefix/lib/foo/node manually.
  [1]

Independent entries are installed concurrently. The slow writer waits for the
fast entry to be installed, so a sequential install would time out.

  $ rm -rf prefix
  $ mkfifo slow fast
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "slow"
  >   "fast"
  > ]
  > EOF
  $ (
  >   while [ ! -f prefix/lib/foo/fast ]; do sleep 0.01; done
  >   printf 'slow\n' >slow
  > ) &
  $ slow_writer=$!
  $ (printf 'fast\n' >fast) &
  $ fast_writer=$!
  $ if $timeout 2 dune install --prefix prefix --display short; then
  >   wait "$slow_writer" "$fast_writer"
  >   grep -qx slow prefix/lib/foo/slow
  >   grep -qx fast prefix/lib/foo/fast
  > else
  >   # Sequential installation blocks on slow and never starts fast.
  >   kill "$slow_writer" "$fast_writer" 2>/dev/null || true
  >   wait "$slow_writer" "$fast_writer" 2>/dev/null || true
  >   false
  > fi
  Installing prefix/lib/foo/slow
  [1]

A warning is not lost when the fallback copy fails. The FIFO is unlinked after
Dune opens it, so parsing can finish but the fallback cannot reopen the source.

  $ rm -rf prefix
  $ mkfifo invalid-meta
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "invalid-meta" {"META"}
  > ]
  > EOF
  $ (exec 3>invalid-meta; rm invalid-meta; printf 'requires = (\n' >&3) &
  $ writer=$!
  $ if dune install --prefix prefix --display short >output 2>&1; then false; fi
  $ wait "$writer"
  $ grep -q 'Warning: Failed to parse file' output
