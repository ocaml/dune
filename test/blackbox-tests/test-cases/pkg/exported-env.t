Packages can export environment variables

  $ export DUNE_CACHE=disabled
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (exported_env
  >  (= FOO bar)
  >  (= BAR xxx)
  >  (+= BAR yyy)
  >  (:= BAR zzz))
  > EOF

  $ make_lockpkg usetest <<'EOF'
  > (depends test)
  > (version 1.2.3)
  > (build
  >  (progn
  >   (system "\| echo FOO=$FOO
  >           "\| echo BAR=$BAR
  >           "\| echo OPAM_PACKAGE_NAME=$OPAM_PACKAGE_NAME
  >           "\| echo OPAM_PACKAGE_VERSION=$OPAM_PACKAGE_VERSION
  >           "\| echo OPAMSWITCH=$OPAMSWITCH
  >           "\| echo "$FOO" > value
  >   )
  >   (write-file usetest.install "share: [\"value\"]")))
  > EOF

  $ build_pkg usetest
  FOO=bar
  BAR=zzz:yyy:xxx
  OPAM_PACKAGE_NAME=usetest
  OPAM_PACKAGE_VERSION=1.2.3
  OPAMSWITCH=dune
  $ cat "$(get_build_pkg_dir usetest)/target/share/usetest/value"
  bar

An unchanged build, including a change to an unrelated ambient variable, does
not rebuild the package.

  $ build_pkg usetest
  $ DUNE_PKG_UNRELATED_ENV=changed build_pkg usetest

Changing a dependency's exports without changing its version or installed
files must update the consumer's installed output.

  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (exported_env (= FOO changed))
  > EOF
  $ build_pkg usetest >build-output 2>&1
  $ cat "$(get_build_pkg_dir usetest)/target/share/usetest/value"
  changed

An expanded export can change even when the lockfile is unchanged. Exercise
both the workspace cache and the shared cache, removing the consumer's target
before each shared-cache build.

  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (exported_env (= FOO %{os_version}))
  > EOF
  $ export DUNE_CACHE_ROOT="$PWD/_cache"

BUG: The second build still uses the first expanded value in both cases.

  $ for cache in disabled enabled; do
  >   export DUNE_CACHE="$cache"
  >   echo "Cache $cache"
  >   for os_version in first second; do
  >     if [ "$cache" = enabled ]; then
  >       rm -rf "$(get_build_pkg_dir usetest)/target"
  >     fi
  >     DUNE_CONFIG__OS_VERSION="$os_version" build_pkg usetest \
  >       >build-output 2>&1
  >     cat "$(get_build_pkg_dir usetest)/target/share/usetest/value"
  >   done
  > done
  Cache disabled
  first
  first
  Cache enabled
  first
  first
