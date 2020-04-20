#!/bin/bash -xue

PATH=~/ocaml/bin:$PATH; export PATH
OPAMYES="true"; export OPAMYES

OPAM_VERSION="2.0.5"

# This allows a specific version of odoc to be selected if necessary
ODOC="odoc>=1.5.0"

TARGET="$1"; shift

# Travis times out after 10mn with no output, which can happen while
# building Coq. This simply outputs something regularly to prevent
# this behavior.
keep_travis_happy () {
    for i in $(seq 15); do sleep 60; echo "Keeping Travis happy ..."; done&
    local job=$!
    "$@"
    kill "$job"
}

opam_install_test_deps () {
    opam install \
         ocamlfind \
         ppxlib \
         ppx_expect \
         "$ODOC" \
         menhir \
         ocaml-migrate-parsetree \
         result.1.4 \
         utop.2.4.2 \
         mdx.1.6.0
    # We install Coq separatedly as to be more resistant w.r.t. the 10
    # minutes Travis timeout; the travis_wait hack doesn't work well
    # with Dune's current setup. Note that Travis caching should help
    # w.r.t. Coq reinstalls; also, in the future we will install a
    # coq-core package which is much more lightweight thus adding less
    # pressure to Dune's CI.
    keep_travis_happy \
        opam install \
        coq.8.11.1
        # js_of_ocaml-ppx \
        # js_of_ocaml-compiler \
}

if [ ${OCAML_VERSION//./} -lt 406 ] ; then
  OLD_OCAML=1
else
  OLD_OCAML=0
fi

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
    if [ $WITH_OPAM -eq 1 -o $OLD_OCAML -eq 1 ] ; then
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
        wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-full-$OPAM_VERSION.tar.gz
        tar -xzf opam-full-$OPAM_VERSION.tar.gz
        cd opam-full-$OPAM_VERSION
        ./configure --prefix=$PREFIX/ocaml
        make lib-ext
        make all
        make install
        cd ../..
        rm -rf src
        rm -rf ~/.opam
        opam init --disable-sandboxing
        eval $(opam config env)

        if [ $OLD_OCAML -eq 1 ] ; then
          opam install ocamlfind-secondary
        fi

        if [ $WITH_OPAM -eq 1 ] ; then
          ./dune.exe runtest && \
              opam_install_test_deps
          opam remove dune jbuilder \
               `opam list --depends-on jbuilder --installed --short` \
               `opam list --depends-on dune     --installed --short`
          if opam info dune &> /dev/null; then
              opam remove dune `opam list --depends-on dune --installed --short`
          fi
        fi
      fi
      cp -a ~/.opam ~/.opam-start
      echo -en "travis_fold:end:opam.init\r"
    fi
  ;;
  build)
    UPDATE_OPAM=0
    RUNTEST_NO_DEPS=runtest-no-deps.out
    echo -en "travis_fold:start:dune.bootstrap\r"
    if [ $OLD_OCAML -eq 1 ] ; then
      eval $(opam env)
    fi
    ocaml bootstrap.ml
    echo -en "travis_fold:end:dune.bootstrap\r"
    if [ $WITH_OPAM -eq 1 ] ; then
      echo -en "travis_fold:start:opam.deps\r"
      DATE=$(date +%Y%m%d)
      eval $(opam env)
      for pkg in $(opam pin list --short); do
        UPDATE_OPAM=1
        opam pin remove $pkg --no-action
        opam remove $pkg || true
      done
      if [ ! -e ~/.opam/last-update ] || [ $(cat ~/.opam/last-update) != $DATE ] ; then
        opam update
        echo $DATE> ~/.opam/last-update
        UPDATE_OPAM=1
        keep_travis_happy opam upgrade
      fi
      if ! ./dune.exe build @runtest-no-deps &> $RUNTEST_NO_DEPS ; then
        cat $RUNTEST_NO_DEPS;
        exit 1;
      fi
      opam list
      version=$(head -1 CHANGES.md |cut -d' ' -f 1)
      for i in *.opam; do
          opam pin add ${i/.opam}.${version} . --no-action
      done
      opam_install_test_deps
      echo -en "travis_fold:end:opam.deps\r"
    fi
    if [ $WITH_OPAM -eq 1 ] ; then
      cat $RUNTEST_NO_DEPS;
      ./dune.exe runtest && \
      # ./dune.exe build @test/blackbox-tests/runtest-js && \
      ./dune.exe build @test/blackbox-tests/runtest-coq && \
      ! ./dune.exe build @test/fail-with-background-jobs-running
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
