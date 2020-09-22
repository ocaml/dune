#!/bin/bash

TERM=st

# Increment whenever the OCaml version or a package is updated to invalidate the caches
SERIAL=1

ROOT_CYG=$(echo $OCAML_ROOT| cygpath -f -)
APPVEYOR_BUILD_FOLDER=$(echo $APPVEYOR_BUILD_FOLDER| cygpath -f -)

ERRORS_ALLOWED=0
function quietly_log {
  if ! script --quiet --return --append --command "$1" $LOG_FILE > /dev/null 2>&1 ; then
    sed -e 's/\d027\[K//g' \
        -e 's/\d027\[m/\d027[0m/g' \
        -e 's/\d027\[01\([m;]\)/\d027[1\1/g' $LOG_FILE
    if ((ERRORS_ALLOWED)) ; then
      return 1
    else
      exit 1
    fi
  fi
}

function msvs_promote_path {
  if [[ ${1%64} = "msvc" ]] ; then
    eval $($ROOT_CYG/msvs-promote-path)
  fi
}

case "$1" in
  install)
    # @@DRA TODO This should be converted OCAML_ROOT to a regexp, not having C:\\\\OCaml hard coded!
    if ! cat $APPVEYOR_BUILD_FOLDER/appveyor.yml | tr -d '\015' | sed -e '1,/^cache:/d' -e '/^$/,$d' | grep -q "^ \+- \+C:\\\\OCaml$" ; then
      echo "$(tput setf 4)ERROR$(tput sgr0) C:\\OCaml doesn't appear to be cached in appveyor.yml"
      exit 1
    fi

    if [[ ! -e $ROOT_CYG/$OCAML_VERSION/$OCAML_PORT/bin/ocamlopt.exe || ! -e $ROOT_CYG/$OCAML_VERSION/version || $(cat $ROOT_CYG/$OCAML_VERSION/version) != "$OCAML_VERSION-$SERIAL" ]] ; then
      if [[ -e $ROOT_CYG/$OCAML_VERSION/version && $(cat $ROOT_CYG/$OCAML_VERSION/version) != "$OCAML_VERSION-$SERIAL" ]] ; then
        echo "Build cache for $OCAML_VERSION has serial $(cat $ROOT_CYG/$OCAML_VERSION/version); should be $OCAML_VERSION-$SERIAL -- clearing"
        rm -rf $ROOT_CYG/$OCAML_VERSION
      elif [[ ! -e $ROOT_CYG/$OCAML_VERSION/version ]] ; then
        rm -rf $ROOT_CYG/$OCAML_VERSION
      fi

      PREFIX=$ROOT_CYG/$OCAML_VERSION/$OCAML_PORT
      ROOT=$(echo $OCAML_ROOT| cygpath -m -f -)/$OCAML_VERSION/$OCAML_PORT
      OCAML_BRANCH=${OCAML_VERSION%.*}
      OCAML_BRANCH=${OCAML_BRANCH/.}

      if [[ ! -d $APPVEYOR_BUILD_FOLDER/../src ]] ; then
        mkdir -p $APPVEYOR_BUILD_FOLDER/../src
        cd $APPVEYOR_BUILD_FOLDER/../src
        git clone https://github.com/ocaml/ocaml.git
        cd ocaml
        mkdir -p $PREFIX
        cp tools/msvs-promote-path $ROOT_CYG/
        cd ..
        FLEXDLL_VER=0.37
        appveyor DownloadFile "https://github.com/alainfrisch/flexdll/releases/download/$FLEXDLL_VER/flexdll-bin-$FLEXDLL_VER.zip" -FileName flexdll-bin-$FLEXDLL_VER.zip
        [[ -e $PREFIX/../version ]] || echo $OCAML_VERSION-$SERIAL> $PREFIX/../version
      fi

      cd $APPVEYOR_BUILD_FOLDER/../src/ocaml
      git checkout $OCAML_VERSION
      git worktree add ../$OCAML_VERSION/$OCAML_PORT/ocaml -b build-$OCAML_VERSION-$OCAML_PORT
      if [[ $OCAML_BRANCH -ge 403 ]] ; then
        pushd ../$OCAML_VERSION/$OCAML_PORT/ocaml > /dev/null
        git submodule update --init
        popd > /dev/null
      fi
      cd ../$OCAML_VERSION/$OCAML_PORT/ocaml
      if [[ ${OCAML_PORT%64} = "cygwin" ]] ; then
        if [[ $OCAML_BRANCH -gt 406 ]] ; then
          NO_ALT_RUNTIMES="-no-instrumented-runtime -no-debug-runtime"
        else
          NO_ALT_RUNTIMES=
        fi
        PRE_WORLD=
        POST_WORLD=
        MAKEFILE=
        ./configure -prefix $PREFIX -no-ocamldoc -no-debugger $NO_ALT_RUNTIMES
      else
        if [[ $OCAML_BRANCH -ge 406 ]] ; then
          cp config/s-nt.h byterun/caml/s.h
          cp config/m-nt.h byterun/caml/m.h
        else
          cp config/s-nt.h config/s.h
          cp config/m-nt.h config/m.h
        fi
        if [[ $OCAML_BRANCH -ge 405 ]] ; then
          POST_WORLD=flexlink.opt
          MAKEFILE=
        else
          POST_WORLD=
          MAKEFILE=-f Makefile.nt
        fi
        if [[ $OCAML_BRANCH -lt 403 ]] ; then
          mkdir -p $PREFIX/bin
          pushd $PREFIX/bin > /dev/null
          case $OCAML_PORT in
            msvc)
              MANIFEST=default.manifest;;
            msvc64)
              MANIFEST=default_amd64.manifest;;
            *)
              MANIFEST=;;
          esac
          unzip $APPVEYOR_BUILD_FOLDER/../src/flexdll-bin-$FLEXDLL_VER.zip flexdll_*$OCAML_PORT.* flexdll.h flexlink.exe $MANIFEST
          popd > /dev/null
          PRE_WORLD=
        else
          PRE_WORLD=flexdll
        fi
        sed -e "s|PREFIX=[^\r]*|PREFIX=$ROOT|" config/Makefile.$OCAML_PORT > config/Makefile
        msvs_promote_path $OCAML_PORT
      fi

      LOG_FILE=OCaml-$OCAML_VERSION-$OCAML_PORT.log
      echo "Building OCaml $OCAML_VERSION for $OCAML_PORT" | tee $LOG_FILE
      echo "Please see $LOG_FILE for further information"
      LOG_FILE="$APPVEYOR_BUILD_FOLDER/$LOG_FILE"
      quietly_log "make $MAKEFILE $PRE_WORLD world.opt $POST_WORLD install"
      # Remove unnecessary executables to keep the build cache size down
      # These are removed here to ensure findlib doesn't configure itself
      # to use .opt commands
      if [[ $OCAML_BRANCH -ge 404 ]] ; then
        if [[ ${OCAML_PORT%64} != "cygwin" ]] ; then
          rm $PREFIX/bin/*.opt.exe
        fi
        rm $PREFIX/bin/*.byte.exe
      else
        for i in $PREFIX/bin/*.opt.exe ; do
          rm ${i%.opt.exe}.exe
          mv $i ${i%.opt.exe}.exe
        done
      fi
      # Remove unnecessary commands to keep the build cache size down
      rm -f $PREFIX/bin/{ocamlcp,ocamldebug,ocamldoc,ocamlmktop,ocamlobjinfo,ocamloptp,ocamlprof}.exe \
            $PREFIX/lib/{expunge,extract_crc,objinfo_helper}.exe
      # Remove unnecessary files
      if [[ $OCAML_BRANCH -lt 405 && $OCAML_BRANCH -gt 402 ]] ; then
        rm $PREFIX/*.txt
      fi
      find $PREFIX -name \*.cmt\* | xargs rm
      find $PREFIX -name \*.ml\* | xargs rm
      rm -f $PREFIX/lib/compiler-libs/*.cmx* $PREFIX/lib/compiler-libs/*.{lib,a} $PREFIX/lib/compiler-libs/ocamloptcomp.cma
      echo "Complete"
      appveyor PushArtifact $(echo $LOG_FILE| cygpath -m -f -)
    fi
    ;;
  build)
    if [[ -z $2 ]] ; then
      set -o pipefail
      SCRIPT=$(echo "$0"| cygpath -f -)
      script -qec "\"$SCRIPT\" $1 script" | sed -e 's/\d027\[K//g' \
                                                -e 's/\d027\[m/\d027[0m/g' \
                                                -e 's/\d027\[01\([m;]\)/\d027[1\1/g'
      exit $?
    fi

    msvs_promote_path $OCAML_PORT

    make -C $APPVEYOR_BUILD_FOLDER all
    ;;
esac
