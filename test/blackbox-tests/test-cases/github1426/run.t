Changing write permissions of a dependency doesn't cause a re-run
  $ chmod -w ./script.sh
  $ dune build a_target --display short
        script a_target
  $ chmod +w ./script.sh
  $ dune build a_target --display short
