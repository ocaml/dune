This checks what happens when a file available in the cache is used in a directory target.

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled

  $ make_directory_targets_project 3.11

  $ cat > dune << EOF
  > (rule
  >  (with-stdout-to file_out (run ./gen.sh)))
  > 
  > (rule
  >  (target (dir dir_out))
  >  (deps gen.sh)
  >  (action
  >   (no-infer
  >    (progn
  >     (run mkdir dir_out)
  >     (with-stdout-to dir_out/a (run ./gen.sh))
  >     (write-file dir_out/b contents_b)))))
  > EOF

  $ cat > gen.sh << EOF
  > #!/usr/bin/env bash
  > >&2 echo running command
  > echo contents
  > EOF
  $ chmod +x gen.sh

We will check whether an entry is linked from the cache. This corresponds to a
file with more than one link.

We prime the cache with the file:

  $ dune build file_out
  running command
  $ is_linked _build/default/file_out
  linked

Then use it in the directory target. We expect the command to run (because we
can not know in advance that it is going to generate that file); but to link
the resulting file from the cache.

  $ dune build dir_out/
  running command
  $ is_linked _build/default/dir_out/a
  linked

Changing a target's declaration from a directory to a file must not reuse the
cached directory, even when the action is unchanged.

  $ export DUNE_TRACE=cache
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir foo))
  >  (action (bash "mkdir foo; echo contents > foo/bar")))
  > EOF
  $ dune build foo

  $ cat > dune <<'EOF'
  > (rule
  >  (target foo)
  >  (action (bash "mkdir foo; echo contents > foo/bar")))
  > EOF
  $ dune build foo
  File "dune", lines 1-3, characters 0-74:
  1 | (rule
  2 |  (target foo)
  3 |  (action (bash "mkdir foo; echo contents > foo/bar")))
  Error: Error trying to read targets after a rule was run:
  - foo: Directory produced for a file target. Use (dir foo).
  [1]
  $ dune trace cat | jq_dune -s '[cacheHitsMatching("/foo$")] | length'
  0
