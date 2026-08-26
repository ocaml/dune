Package version numbers in lock-directory paths are a writer-only choice. Both
layouts must load and produce the same build plan.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat > data.txt <<'EOF'
  > layout invariant
  > EOF
  $ data_checksum=$(md5sum data.txt | cut -d' ' -f1)

  $ mkpkg layout 1.2 <<EOF
  > build: [
  >  ["sh" "-c" "mkdir -p %{share}% && cp data.txt %{share}%/result"]
  > ]
  > extra-files: ["data.txt" "md5=$data_checksum"]
  > EOF
  $ mkdir -p "$mock_packages/layout/layout.1.2/files"
  $ cp data.txt "$mock_packages/layout/layout.1.2/files/data.txt"

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends layout))
  > EOF

Invalid values are rejected by the writer.

  $ DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=other dune pkg lock
  Error: Invalid value "other" for DUNE_PKG_VERSIONED_LOCK_DIR_PATHS.
  Hint: Expected "enabled" or "disabled".
  [1]

Generate the same solution with each package path layout.

  $ DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=enabled dune_pkg_lock_normalized >/dev/null
  $ mv dune.lock versioned.lock
  $ test -f versioned.lock/layout.1.2.pkg
  $ test -f versioned.lock/layout.1.2.files/data.txt

  $ DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=disabled dune_pkg_lock_normalized >/dev/null
  $ mv dune.lock unversioned.lock
  $ test -f unversioned.lock/layout.pkg
  $ test -f unversioned.lock/layout.files/data.txt

The reader must detect the layout from the lock directory rather than consulting
the writer flag. Build each lock directory with the opposite flag value.

  $ cp -R versioned.lock dune.lock
  $ versioned_digest=$(DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=disabled dune pkg print-digest layout)
  $ DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=disabled build_pkg layout
  $ cp "$(get_build_pkg_dir layout)/target/share/result" versioned-result

  $ rm -rf dune.lock
  $ cp -R unversioned.lock dune.lock
  $ dune clean
  $ unversioned_digest=$(DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=enabled dune pkg print-digest layout)
  $ DUNE_PKG_VERSIONED_LOCK_DIR_PATHS=enabled build_pkg layout
  $ cp "$(get_build_pkg_dir layout)/target/share/result" unversioned-result

  $ test "$versioned_digest" = "$unversioned_digest" && echo 'package digests match'
  package digests match
  $ cmp versioned-result unversioned-result && cat versioned-result
  layout invariant
