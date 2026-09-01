The number of opam files read while solving is much larger than the size of
the solution: the solver reads every version of every package name it touches.

Build a repository with a chain of 10 packages, each having 20 versions:

  $ export DUNE_TRACE=+sat
  $ rm -rf "${source_lock_dir}"
  $ for i in $(seq 1 10); do
  >   for v in $(seq 1 20); do
  >     if [ "$i" -eq 10 ]; then
  >       mkpkg "p$i" "0.0.$v" << EOF
  > EOF
  >     else
  >       mkpkg "p$i" "0.0.$v" << EOF
  > depends: [ "p$((i + 1))" ]
  > EOF
  >     fi
  >   done
  > done

  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends p1))
  > EOF
  Solution for dune.lock:
  - p1.0.0.20
  - p10.0.0.20
  - p2.0.0.20
  - p3.0.0.20
  - p4.0.0.20
  - p5.0.0.20
  - p6.0.0.20
  - p7.0.0.20
  - p8.0.0.20
  - p9.0.0.20

10 packages are selected, but 200 opam files were read:

  $ dune trace cat | jq -s 'include "dune"; [ .[] | satSolveEvents | .args.num_opam_files ]'
  [
    200
  ]
