Package build and install commands can currently overwrite an installed
artifact of a dependency outside the sandbox.

  $ make_lockdir
  $ make_dune_project 3.25
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

  $ unset DUNE_CONFIG__LANDLOCK
  $ make_lockpkg protected <<'EOF'
  > (version dev)
  > (install (run sh -c "echo original > \"$1\"" sh %{pkg-self:share}/payload))
  > EOF
  $ build_pkg protected
  $ payload="$PWD/$(get_build_pkg_dir protected)/target/share/protected/payload"
  $ chmod u+w "$payload"
  $ make_lockpkg writer <<EOF
  > (version dev)
  > (depends protected)
  > (build (run sh probe.sh "$payload" build))
  > (install (run sh probe.sh "$payload" %{pkg-self:share}/installed))
  > EOF
  $ make_lockpkg_file writer probe.sh <<'EOF'
  > set -eu
  > test -r "$1"
  > echo local > "$2"
  > if (echo overwritten > "$1") 2>/dev/null; then
  >   echo wrote
  > else
  >   echo blocked
  > fi
  > EOF
  $ build_pkg writer
  wrote
  wrote
  $ cat "$payload"
  overwritten
  $ cat "$(get_build_pkg_dir writer)/target/share/writer/installed"
  local
