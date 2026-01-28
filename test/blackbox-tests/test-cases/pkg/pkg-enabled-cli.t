# Test the behaviour of the --pkg enabled/disabled command line option.

  $ cat > dune-workspace.enabled << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

  $ cat > dune-workspace.disabled << EOF
  > (lang dune 3.20)
  > (pkg disabled)
  > EOF

## No lock directory

Default behaviour is disabled, as there is no lock directory
  $ dune pkg enabled
  [1]
  $ dune pkg enabled --workspace=./dune-workspace.enabled
  $ dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

With the CLI option enabled
  $ dune pkg enabled --pkg enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.enabled 
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.disabled 

With the CLI option disabled
  $ dune pkg enabled --pkg disabled
  [1]
The CLI option has priority over the workspace file
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.disabled
  [1]

## With a lock directory present
  $ make_lockdir

Default behaviour is enabled, as a lockdir is detected
  $ dune pkg enabled
  $ dune pkg enabled --workspace=./dune-workspace.enabled
  $ dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

With the CLI option enabled
  $ dune pkg enabled --pkg enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.disabled

With the CLI option disabled
  $ dune pkg enabled --pkg disabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.disabled
  [1]

## With a lock directory, but we ignore it

  $ test -d dune.lock

  $ dune pkg enabled --ignore-lock-dir
^^^ This is a bug. It should be disabled.
We have a lock directory, but it is ignored.
It should behave the same as if there was no lockdir

This translates to explicitly asking for autolocking
  $ dune pkg enabled --ignore-lock-dir --workspace=./dune-workspace.enabled
  $ dune pkg enabled --ignore-lock-dir --workspace=./dune-workspace.disabled
  [1]

These 3 translate to explicitly asking for autolocking
  $ dune pkg enabled --pkg enabled --ignore-lock-dir
  $ dune pkg enabled --pkg enabled --ignore-lock-dir --workspace=./dune-workspace.enabled
  $ dune pkg enabled --pkg enabled --ignore-lock-dir --workspace=./dune-workspace.disabled

  $ dune pkg enabled --pkg disabled --ignore-lock-dir
  [1]
  $ dune pkg enabled --pkg disabled --ignore-lock-dir --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled --pkg disabled --ignore-lock-dir --workspace=./dune-workspace.disabled
  [1]

## With a project file

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > (package (name foo) (allow_empty))
  > EOF

The '-p' option implies --pkg=disabled, and it has priority over the workspace
  $ dune pkg enabled -p foo --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled -p foo --workspace=./dune-workspace.disabled
  [1]
