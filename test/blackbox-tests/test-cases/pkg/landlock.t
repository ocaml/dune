On systems with Landlock, package build and install commands cannot overwrite
an installed dependency artifact outside the sandbox.

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

Landlock can be unavailable even on Linux, depending on the kernel.

  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   status=blocked
  >   contents=original
  > else
  >   status=wrote
  >   contents=overwritten
  > fi
  $ build_pkg writer > writer.log 2>&1
  $ printf '%s\n' "$status" "$status" > expected.log
  $ cmp expected.log writer.log
  $ test "$(cat "$payload")" = "$contents"
  $ cat "$(get_build_pkg_dir writer)/target/share/writer/installed"
  local
