Testing `dune fmt` over RPC from a client subdirectory. Reproduces
#15107 (sub-issue of #12230).

When a client invokes `dune fmt` from a subdir while a watch server is
running, the request sent over RPC carries no cwd information: the
server re-issues the build as `Alias_rec "fmt"` at its own workspace
root, formatting whatever is reachable from there rather than just the
user's subdirectory.

CR-someday Alizter: unlike the runtest RPC test we cannot use `--root`
here. `--root` forces `to_cwd = []` (see bin/workspace_root.ml:91), so
both client and server end up with `reach_from_root_prefix = ""` and
the local fmt code also expands to the workspace root, masking the
mismatch. Whether that `--root` behaviour is itself by-design or a
latent issue is its own question. We use walk-up workspace discovery
anchored by a dune-workspace file at $TESTCASE_ROOT, which requires
unsetting INSIDE_DUNE (cram sets it so dune treats cwd as workspace,
which is the wrong setup for this test). The dune-workspace anchor
ensures the walk-up stops at $TESTCASE_ROOT rather than escaping the
sandbox.

  $ unset INSIDE_DUNE
  $ make_dune_project 3.25
  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > EOF

Add a custom rule under the @fmt alias in two distinct directories so
we can observe which one(s) were actually built.

  $ mkdir subdir other
  $ cat > subdir/dune <<EOF
  > (rule
  >  (alias fmt)
  >  (target ran-here)
  >  (action (write-file %{target} "ran in subdir")))
  > EOF
  $ cat > other/dune <<EOF
  > (rule
  >  (alias fmt)
  >  (target ran-here)
  >  (action (write-file %{target} "ran in other")))
  > EOF

Sanity check the local code path: without a watch server, running
`dune fmt` from `subdir` scopes the @@fmt alias to subdir/. `dune fmt`
exits 1 because there are unformatted dune files; we only care that
the subdir's rule fired and the other directory's did not.

  $ (cd subdir && dune fmt) > /dev/null 2>&1
  [1]
  $ test -f _build/default/subdir/ran-here && echo "subdir built" || echo "subdir NOT built"
  subdir built
  $ test -f _build/default/other/ran-here && echo "other built" || echo "other NOT built"
  other NOT built

  $ dune clean

Start a passive watch server at the workspace root.

  $ start_dune

CR-someday Alizter: from `subdir` the user typed `dune fmt`, meaning
"format what's under here". Today the RPC payload is unit; the server
runs @@fmt at the workspace root and builds *both* the subdir rule and
the unrelated `other` rule.

  $ (cd subdir && dune fmt)
  $ test -f _build/default/subdir/ran-here && echo "subdir built" || echo "subdir NOT built"
  subdir built
  $ test -f _build/default/other/ran-here && echo "other built" || echo "other NOT built"
  other built

  $ stop_dune
  Warning:
  Your build request is being forwarded to a running Dune instance. Note that
  certain command line arguments may be ignored.
  File "other/dune", line 1, characters 0-0:
  ------ other/dune
  ++++++ other/dune.corrected
  File "other/dune", line 4, characters 0-1:
   |(rule
   | (alias fmt)
   | (target ran-here)
  -| (action (write-file %{target} "ran in other")))
  +| (action
  +|  (write-file %{target} "ran in other")))
  Had 1 error, waiting for filesystem changes...
  Promoting _build/default/other/dune.corrected to other/dune.
