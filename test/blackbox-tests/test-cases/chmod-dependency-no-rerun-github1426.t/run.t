Changing write permissions of a dependency doesn't cause a re-run

  $ checkrun() {
  > dune build a_target
  > dune trace cat | jq_dune -c '
  > progMatching("script")
  > '
  > }

  $ chmod -w ./script.sh
  $ checkrun
  {"prog":"script.sh","process_args":[],"target_files":["_build/default/a_target"]}
  $ chmod +w ./script.sh
  $ checkrun
