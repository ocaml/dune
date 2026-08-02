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

The following helper checks that the second entry does not start while the
first is blocked reading its source.

  $ second_entry_must_not_start() {
  >   source=$1
  >   second_destination=$2
  >   mkfifo "$source"
  >   (
  >     while ! grep -qx second "$second_destination" 2>/dev/null; do
  >       sleep 0.01
  >     done
  >     printf 'first\n' >"$source"
  >   ) &
  >   writer=$!
  >   if output=$(
  >     $timeout --signal=KILL 2 dune install --prefix prefix \
  >       --display short 2>&1
  >   ); then
  >     printf '%s\n' "$output"
  >     wait "$writer"
  >     return 1
  >   else
  >     printf '%s\n' "$output"
  >     kill "$writer" 2>/dev/null || true
  >     wait "$writer" 2>/dev/null || true
  >   fi
  > }

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

Artifact-substitution staging paths also conflict with declared destinations.
The copy to `target` stages its contents at `.#target.dune-temp`. The writer
waits for that destination to be installed, so a sequential install times out.

  $ rm -rf prefix
  $ printf 'second\n' >second
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "staging-source" {"target"}
  >   "second" {".#target.dune-temp"}
  > ]
  > EOF
  $ second_entry_must_not_start staging-source \
  >   prefix/lib/foo/.#target.dune-temp
  Installing prefix/lib/foo/target

Destinations that resolve through symlinks can also refer to the same file.
The writer waits for the second alias, so preserving sequential behavior makes
the install time out before it can start the second entry.

  $ rm -rf prefix
  $ mkdir -p prefix/lib/foo/shared
  $ ln -s shared prefix/lib/foo/first-alias
  $ ln -s shared prefix/lib/foo/second-alias
  $ printf 'second\n' >second
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "alias-source" {"first-alias/target"}
  >   "second" {"second-alias/target"}
  > ]
  > EOF
  $ second_entry_must_not_start alias-source prefix/lib/foo/shared/target
  Installing prefix/lib/foo/first-alias/target

A dangling symlink may start resolving when another entry creates its target
directory. These destinations must also remain sequential.

  $ rm -rf prefix
  $ mkdir -p prefix/lib/foo
  $ ln -s future-dir prefix/lib/foo/future-alias
  $ printf 'second\n' >second
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "future-source" {"future-dir/target"}
  >   "second" {"future-alias/target"}
  > ]
  > EOF
  $ second_entry_must_not_start future-source prefix/lib/foo/future-dir/target
  Installing prefix/lib/foo/future-dir/target

Case variants are conservatively processed sequentially so that they cannot
race on case-insensitive filesystems.

  $ rm -rf prefix
  $ printf 'second\n' >second
  $ cat >_build/default/foo.install <<EOF
  > lib: [
  >   "case-source" {"case-target"}
  >   "second" {"CASE-TARGET"}
  > ]
  > EOF
  $ second_entry_must_not_start case-source prefix/lib/foo/CASE-TARGET
  Installing prefix/lib/foo/case-target

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
  $ if output=$(
  >   $timeout --signal=KILL 10 dune install --prefix prefix \
  >     --display short 2>&1
  > ); then
  >   printf '%s\n' "$output"
  >   wait "$slow_writer" "$fast_writer"
  >   grep -qx slow prefix/lib/foo/slow
  >   grep -qx fast prefix/lib/foo/fast
  > else
  >   printf '%s\n' "$output"
  >   # Sequential installation blocks on slow and never starts fast.
  >   kill "$slow_writer" "$fast_writer" 2>/dev/null || true
  >   wait "$slow_writer" "$fast_writer" 2>/dev/null || true
  >   false
  > fi
  Installing prefix/lib/foo/slow
  Installing prefix/lib/foo/fast

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
