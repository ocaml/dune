Testing `dune show env` with absolute path arguments. Reproduces #15104
(sub-issue of #12230).

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (env
  >  (_ (flags (:standard -w +27))))
  > EOF

  $ mkdir subdir

A relative path from the workspace root works. We filter to the
user-added `+27` so the test is not brittle to changes in the
`:standard` warning expansion:

  $ dune show env . --field flags | grep -oE '\+27'
  +27

CR-someday Alizter: an absolute path pointing at the same directory should
work identically to the relative form. Today it errors as if the path were
outside the project.

  $ dune show env $PWD --field flags
  (flags
   (-short-paths -keep-locs -warn-error +a -w +27))

CR-someday Alizter: the same call from a subdirectory should also resolve.
(--root is required because INSIDE_DUNE disables workspace auto-detection.)

  $ (cd subdir && dune show env --root .. $PWD --field flags)
  (flags
   (-short-paths -keep-locs -warn-error +a -w +27))

CR-someday Alizter: absolute paths to subdirectories of the workspace
should be accepted.

  $ dune show env $PWD/subdir --field flags
  (flags
   (-short-paths -keep-locs -warn-error +a -w +27))

Absolute paths that are genuinely outside the workspace must continue to
fail with a clean error. This behaviour must not regress.

  $ dune show env /tmp --field flags
  Error: Environment is not defined for external paths
  [1]
