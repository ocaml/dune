Testing `dune build` over RPC with a target supplied from a client
subdirectory. Reproduces #15106 (sub-issue of #12230).

When a client invokes `dune build <relative-target>` from a subdir
while a watch server is running, the target name is sent verbatim over
RPC and the server resolves it against its own workspace-root offset,
not the client's cwd.

This test passes `--root` on the client side: that forces the client's
`to_cwd = []` (see bin/workspace_root.ml), so the relative target is
sent as-is. A future fix will need to either resolve the target
client-side or extend the RPC build request to carry the client's cwd
— see the discussion on #15106.

  $ make_dune_project 3.25

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (executable (name foo))
  > EOF
  $ cat > subdir/foo.ml <<EOF
  > let () = print_endline "hi"
  > EOF

Start a passive watch server at the workspace root.

  $ start_dune

CR-someday Alizter: from `subdir` the user typed `foo.exe`, meaning
`subdir/foo.exe`. Today the target is sent verbatim over RPC and the
server resolves it at the workspace root via its own
`reach_from_root_prefix`, so the build fails because there is no
`foo.exe` at the workspace root.

  $ root=$PWD
  $ (cd subdir && dune build --root "$root" foo.exe)
  Entering directory '..'
  Error: Don't know how to build foo.exe
  Error: Build failed with 1 error.
  Leaving directory '..'
  [1]

  $ stop_dune
  Error: Don't know how to build foo.exe
  Had 1 error, waiting for filesystem changes...
