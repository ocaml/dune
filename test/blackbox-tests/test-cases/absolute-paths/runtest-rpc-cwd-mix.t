Testing `dune runtest` over RPC with a path supplied from a client
subdirectory. Reproduces #15105 (sub-issue of #12230).

When a client invokes `dune runtest <relative-path>` from a subdir while
a watch server is running, the relative path is sent verbatim over RPC
and the server interprets it without the client's cwd context — so a
test the user named relative to their own directory is not found.

This test passes `--root` on the client side: that forces the client's
`to_cwd = []` (see bin/workspace_root.ml), which would normally mask
the bug. The fix routes via absolute paths
(`Arg.Workspace_path.absolute` client-side, `try_localize_external`
server-side) and so bypasses the `to_cwd` channel entirely — meaning
the bug surfaces and the fix lands cleanly even under `--root`.

  $ make_dune_project 3.25

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (cram (deps mytest.t))
  > EOF
  $ cat > subdir/mytest.t <<EOF
  >   $ echo "I am the test in subdir"
  > EOF

Start a passive watch server at the workspace root.

  $ start_dune

CR-someday Alizter: from `subdir` the user typed `mytest.t`, meaning
`subdir/mytest.t`. Today the path is sent verbatim over RPC and the
server resolves it at the workspace root rather than against the
client's cwd.

  $ root=$PWD
  $ (cd subdir && dune runtest --root "$root" mytest.t)
  Entering directory '..'
  Error: "mytest.t" does not match any known test.
  Error: Build failed with 1 error.
  Leaving directory '..'
  [1]

  $ stop_dune
  Error: "mytest.t" does not match any known test.
  Had 1 error, waiting for filesystem changes...
