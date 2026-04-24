# Test the behaviour of the DUNE_PKG env variable, which mirrors the --pkg CLI option.

  $ unset DUNE_PKG

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

With the DUNE_PKG env variable enabled
  $ DUNE_PKG=enabled dune pkg enabled
  $ DUNE_PKG=enabled dune pkg enabled --workspace=./dune-workspace.enabled
  $ DUNE_PKG=enabled dune pkg enabled --workspace=./dune-workspace.disabled

With the DUNE_PKG env variable disabled
  $ DUNE_PKG=disabled dune pkg enabled
  [1]
The DUNE_PKG env variable has priority over the workspace file
  $ DUNE_PKG=disabled dune pkg enabled --workspace=./dune-workspace.enabled
  [1]
  $ DUNE_PKG=disabled dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

## With a lock directory present
  $ make_lockdir

Default behaviour is enabled, as a lockdir is detected
  $ dune pkg enabled
  $ dune pkg enabled --workspace=./dune-workspace.enabled
  $ dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

With the DUNE_PKG env variable enabled
  $ DUNE_PKG=enabled dune pkg enabled
  $ DUNE_PKG=enabled dune pkg enabled --workspace=./dune-workspace.enabled
  $ DUNE_PKG=enabled dune pkg enabled --workspace=./dune-workspace.disabled

With the DUNE_PKG env variable disabled
  $ DUNE_PKG=disabled dune pkg enabled
  [1]
  $ DUNE_PKG=disabled dune pkg enabled --workspace=./dune-workspace.enabled
  [1]
  $ DUNE_PKG=disabled dune pkg enabled --workspace=./dune-workspace.disabled
  [1]

The CLI option should take priority over the DUNE_PKG env variable
  $ DUNE_PKG=enabled dune pkg enabled --pkg disabled
  [1]
  $ DUNE_PKG=disabled dune pkg enabled --pkg enabled

## With a project file

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > (package (name foo) (allow_empty))
  > EOF

The '-p' option implies --pkg=disabled, which overrides the DUNE_PKG env variable.
  $ DUNE_PKG=enabled dune pkg enabled -p foo
  [1]
