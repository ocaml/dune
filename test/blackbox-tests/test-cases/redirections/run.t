  $ $JBUILDER runtest -j1 --display short --root . 2>&1 | sed "s/ cmd /  sh /"
            sh stderr,stdout
            sh both
            sh stderr,stdout
            sh both
          diff alias runtest
          diff alias runtest
          diff alias runtest
