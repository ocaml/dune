Packages can export environment variables

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (exported_env
  >  (= FOO bar)
  >  (= BAR xxx)
  >  (+= BAR yyy)
  >  (:= BAR zzz))
  > EOF

  $ cat >dune.lock/usetest.pkg <<'EOF'
  > (deps test)
  > (version 1.2.3)
  > (build
  >  (progn
  >   (system "\| echo FOO=$FOO
  >           "\| echo BAR=$BAR
  >           "\| echo OPAM_PACKAGE_NAME=$OPAM_PACKAGE_NAME
  >           "\| echo OPAM_PACKAGE_VERSION=$OPAM_PACKAGE_VERSION
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ dune build .pkg/usetest/target/
  FOO=bar
  BAR=zzz:yyy:xxx
  OPAM_PACKAGE_NAME=usetest
  OPAM_PACKAGE_VERSION=1.2.3
