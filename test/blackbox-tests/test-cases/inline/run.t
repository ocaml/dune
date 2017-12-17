  $ cat jbuild.in > jbuild
  $ cat jbuild
  (jbuild_version 1)
  
  (inline (echo "toto"))
  (end)
  $ $JBUILDER build --root . -j1 --diff-command false @jbuild
            sh (internal) (exit 1)
  /bin/sh -c 'false '\''jbuild.old'\'' '\''jbuild'\'''
  [1]
  $ cat jbuild
  (jbuild_version 1)
  
  (inline (echo "toto"))
  toto(end)
