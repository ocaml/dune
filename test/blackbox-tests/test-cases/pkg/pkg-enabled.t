Exercise the "dune pkg enabled" command which checks whether package management
should be used.

  $ . ./helpers.sh

  $ mkrepo

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (lock_dir
  >  (path dune.other.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

When no lockdir is present pkg is not enabled:
  $ dune pkg enabled
  [1]

When the default lockdir is present pkg is enabled:
  $ dune pkg lock > /dev/null 2> /dev/null

  $ dune pkg enabled

  $ rm -r ${default_lock_dir}

When a non-default lockdir is present, pkg is still enabled:
  $ dune pkg lock dune.other.lock > /dev/null 2> /dev/null
  $ dune pkg enabled

Remove the other lock dir and make sure the status didn't latch to be always
enabled.

  $ rm -r dune.other.lock
  $ dune pkg enabled
  [1]

Manually enable package management in the workspace, it should be reported as
enabled:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF
  $ dune pkg enabled && echo "Yes, it is enabled"
  Yes, it is enabled

If we remove the setting from the workspace it should go back to the default
(disabled)

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > EOF
  $ dune pkg enabled || echo "Package management disabled"
  Package management disabled

Enable the package management globally in the user's config.

  $ cat > config <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF
  $ dune pkg enabled --config-file=config && echo "Yes, it is enabled"
  Yes, it is enabled

Disable it in the user config, but enable it in the workspace. Workspace is
higher precedence so it should be enabled:

  $ cat > config <<EOF
  > (lang dune 3.20)
  > (pkg disabled)
  > EOF
  $ dune pkg enabled --config-file=config || echo "Successfully disabled by config"
  Successfully disabled by config
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF
  $ dune pkg enabled --config-file=config && echo "Workspace config overrides user config"
  Workspace config overrides user config
