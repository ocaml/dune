Platform-specific package alternatives must each preserve their extra files in the
portable lock directory. Files from an unavailable alternative must not be copied.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ LINUX_FILE=mock-opam-repository/packages/linux-impl/linux-impl.1/files/platform.txt
  $ MACOS_FILE=mock-opam-repository/packages/macos-impl/macos-impl.1/files/platform.txt
  $ UNUSED_FILE=mock-opam-repository/packages/unused-impl/unused-impl.1/files/platform.txt

  $ mkdir -p "$(dirname "$LINUX_FILE")"
  $ echo linux >"$LINUX_FILE"
  $ mkdir -p "$(dirname "$MACOS_FILE")"
  $ echo macos >"$MACOS_FILE"
  $ mkdir -p "$(dirname "$UNUSED_FILE")"
  $ echo unused >"$UNUSED_FILE"

  $ mkpkg linux-impl 1 <<EOF
  > available: os = "linux"
  > extra-files: [
  >   ["platform.txt" "md5=$(md5sum "$LINUX_FILE" | cut -f1 -d' ')"]
  > ]
  > EOF
  $ mkpkg macos-impl 1 <<EOF
  > available: os = "macos"
  > extra-files: [
  >   ["platform.txt" "md5=$(md5sum "$MACOS_FILE" | cut -f1 -d' ')"]
  > ]
  > EOF
  $ mkpkg unused-impl 1 <<EOF
  > available: false
  > extra-files: [
  >   ["platform.txt" "md5=$(md5sum "$UNUSED_FILE" | cut -f1 -d' ')"]
  > ]
  > EOF

  $ cat >dune-project <<'EOF'
  > (lang dune 3.18)
  > EOF
  $ cat >x.opam <<'EOF'
  > opam-version: "2.0"
  > depends: [ "linux-impl" | "macos-impl" | "unused-impl" ]
  > EOF

  $ dune pkg lock >/dev/null 2>&1
  $ cat dune.lock/linux-impl.1.files/platform.txt
  linux
  $ cat dune.lock/macos-impl.1.files/platform.txt
  macos
  $ test ! -e dune.lock/unused-impl.1.files
