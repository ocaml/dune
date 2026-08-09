Evaluates `%{lib-available:...}` forms in actions.

  $ dune build @runtest --display short --debug-dependency-path 2>&1 | sed "s/ cmd /  sh /"
            sh (anonymous)
            sh (anonymous)

Anonymous actions are cached without creating target files.

  $ if [ -d _build/.actions ]; then find _build/.actions -type f; fi

  $ dune build @runtest
