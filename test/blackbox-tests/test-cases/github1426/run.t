  $ chmod -x ./script.sh
  $ dune build a_target
        script a_target (exit 127)
  (cd _build/default && ./script.sh) > _build/default/a_target
  [1]
  $ chmod +x ./script.sh
  $ dune build a_target
  $ chmod -x ./script.sh
