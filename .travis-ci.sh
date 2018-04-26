#!/bin/bash -xue

PATH=~/ocaml/bin:$PATH; export PATH

TARGET="$1"; shift

case "$TARGET" in
  prepare)
    echo -en "travis_fold:start:ocaml\r"
    if [ ! -e ~/ocaml/cached-version -o "$(cat ~/ocaml/cached-version)" != "$OCAML_VERSION.$OCAML_RELEASE" ] ; then
      rm -rf ~/ocaml
      mkdir -p ~/ocaml/src
      cd ~/ocaml/src
      wget http://caml.inria.fr/pub/distrib/ocaml-$OCAML_VERSION/ocaml-$OCAML_VERSION.$OCAML_RELEASE.tar.gz
      tar -xzf ocaml-$OCAML_VERSION.$OCAML_RELEASE.tar.gz
      cd ocaml-$OCAML_VERSION.$OCAML_RELEASE
      ./configure -prefix ~/ocaml
      make world.opt
      make install
      cd ../..
      rm -rf src
      echo "$OCAML_VERSION.$OCAML_RELEASE" > ~/ocaml/cached-version
    fi
    echo -en "travis_fold:end:ocaml\r"
    if [ $WITH_OPAM -eq 1 ] ; then
      echo -en "travis_fold:start:opam.init\r"
      if [ "$TRAVIS_OS_NAME" = "osx" ] ; then
        brew update
        brew install aspcud
        PREFIX=/Users/travis
      else
        PREFIX=/home/travis
      fi
      if [ ! -e ~/ocaml/bin/opam -o ! -e ~/.opam/lock -o "$OPAM_RESET" = "1" ] ; then
        mkdir ~/ocaml/src
        cd ~/ocaml/src
        wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-full-1.2.2.tar.gz
        tar -xzf opam-full-1.2.2.tar.gz
        cd opam-full-1.2.2
        ./configure --prefix=$PREFIX/ocaml
        make lib-ext
        make all
        make install
        cd ../..
        rm -rf src
        rm -rf ~/.opam
        opam init --yes
        eval $(opam config env)
        opam install utop ppx_driver odoc menhir ocaml-migrate-parsetree js_of_ocaml-ppx js_of_ocaml-compiler--yes
        opam remove jbuilder `opam list --depends-on jbuilder --installed --short` --yes
        if opam info dune &> /dev/null; then
            opam remove dune `opam list --depends-on dune --installed --short` --yes
        fi
      fi
      cp -a ~/.opam ~/.opam-start
      echo -en "travis_fold:end:opam.init\r"
    fi
  ;;
  build)
    UPDATE_OPAM=0
    if [ $WITH_OPAM -eq 1 ] ; then
      echo -en "travis_fold:start:opam.deps\r"
      DATE=$(date +%Y%m%d)
      eval $(opam config env)
      if [ $(opam pin list | wc -l) -ne 0 ] ; then
        UPDATE_OPAM=1
        opam pin remove jbuilder --no-action --yes
        opam remove jbuilder --yes
        if opam pin list -s | grep dune; then
            opam pin remove dune --no-action --yes
            opam remove dune --yes
        fi
      fi
      if [ ! -e ~/.opam/last-update ] || [ $(cat ~/.opam/last-update) != $DATE ] ; then
        opam update --yes
        echo $DATE> ~/.opam/last-update
        UPDATE_OPAM=1
        opam upgrade --yes
      fi
      opam list
      echo "version: \"1.0+dev$DATE\"" >> dune.opam
      cat > jbuilder.opam <<EOF
version: "1.0+dev$DATE"
opam-version: "1.2"
maintainer: "opensource@janestreet.com"
authors: ["Jane Street Group, LLC <opensource@janestreet.com>"]
homepage: "https://github.com/ocaml/dune"
bug-reports: "https://github.com/ocaml/dune/issues"
dev-repo: "https://github.com/ocaml/dune.git"
license: "Apache-2.0"
depends: [ "dune" ]
EOF
      opam pin add dune     . --no-action --yes
      opam pin add jbuilder . --no-action --yes
      opam install utop ppx_driver odoc ocaml-migrate-parsetree js_of_ocaml-ppx js_of_ocaml-compiler --yes
      echo -en "travis_fold:end:opam.deps\r"
    fi
    echo -en "travis_fold:start:dune.bootstrap\r"
    ocaml bootstrap.ml
    echo -en "travis_fold:end:dune.bootstrap\r"
    ./boot.exe --subst
    echo -en "travis_fold:start:dune.boot\r"
    ./boot.exe --dev
    echo -en "travis_fold:end:dune.boot\r"
    if [ $WITH_OPAM -eq 1 ] ; then
      _build/install/default/bin/dune runtest && \
      _build/install/default/bin/dune build @test/blackbox-tests/runtest-js && \
      ! _build/install/default/bin/dune build @test/fail-with-background-jobs-running
      RESULT=$?
      if [ $UPDATE_OPAM -eq 0 ] ; then
        rm -rf ~/.opam
        mv ~/.opam-start ~/.opam
      fi
      exit $RESULT
    fi
  ;;
  *)
    echo "bad command $TARGET">&2; exit 1
esac
