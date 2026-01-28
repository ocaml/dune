# Test the behaviour of the --pkg enabled/autolocking/disabled command line option.

  $ cat > dune-workspace.enabled << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

  $ cat > dune-workspace.autolocking << EOF
  > (lang dune 3.20)
  > (pkg autolocking)
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
Auto means we don't need the lockdir to be present, so it's enabled
  $ dune pkg enabled --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

With the CLI option enabled, it has priority over the workspace file
  $ dune pkg enabled --pkg enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.enabled 
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.disabled 

With the CLI option set to auto, it has priority over the workspace file
  $ dune pkg enabled --pkg autolocking
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.enabled
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.disabled

With the CLI option disabled
  $ dune pkg enabled --pkg disabled
  [1]
The CLI option has priority over the workspace file
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.autolocking
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.disabled
  [1]

## With a lock directory present
  $ make_lockdir

Default behaviour is enabled, as a lockdir is detected
  $ dune pkg enabled
  $ dune pkg enabled --workspace=./dune-workspace.enabled
Autolocking still means enabled
  $ dune pkg enabled --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

With the CLI option enabled
  $ dune pkg enabled --pkg enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.enabled
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --pkg enabled --workspace=./dune-workspace.disabled

With the CLI option set to auto
  $ dune pkg enabled --pkg autolocking
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.enabled
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.autolocking
  $ dune pkg enabled --pkg autolocking --workspace=./dune-workspace.disabled

With the CLI option disabled
  $ dune pkg enabled --pkg disabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.autolocking
  [1]
  $ dune pkg enabled --pkg disabled --workspace=./dune-workspace.disabled
  [1]

## With a project file

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > (package (name foo) (allow_empty))
  > EOF

The '-p' option implies --pkg=disabled, and it has priority over the workspace
  $ dune pkg enabled -p foo --workspace=./dune-workspace.enabled
  [1]
  $ dune pkg enabled -p foo --workspace=./dune-workspace.autolocking
  [1]
  $ dune pkg enabled -p foo --workspace=./dune-workspace.disabled
  [1]
