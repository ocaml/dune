Changing parallelism must not rerun an unchanged package action. Show only the
arguments from its process-start trace, without producing execution logs.

  $ make_dune_project 3.26
  $ make_lockdir
  $ mkdir src
  $ echo original > src/input
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (source (copy $PWD/src))
  > (build (run sh -c : %{jobs} -j%{jobs} (concat n %{jobs}) 2))
  > EOF
  $ arguments() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "process" and .name == "start")
  >       | select(.args.process_args[0:2] == ["-c", ":"])
  >       | .args.process_args[2:]
  >     ]
  >   '
  > }
  $ target=$(get_build_pkg_dir foo)/target
  $ dune build -j 1 "$target"
  $ arguments
  [["1","-j1","n1","2"]]
  $ dune build -j 1 "$target"
  $ arguments
  []
  $ dune build -j 2 "$target"
  $ arguments
  [["2","-j2","n2","2"]]

DUNE_JOBS currently causes the same unnecessary invalidation as -j.

  $ DUNE_JOBS=3 dune build "$target"
  $ arguments
  [["3","-j3","n3","2"]]

A real source change must rerun the action with the current job count.

  $ echo changed > src/input
  $ dune build -j 4 "$target"
  $ arguments
  [["4","-j4","n4","2"]]

Literal numeric arguments remain part of the build key.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (source (copy $PWD/src))
  > (build (run sh -c : %{jobs} -j%{jobs} (concat n %{jobs}) 3))
  > EOF
  $ build_pkg foo
  $ arguments
  [["1","-j1","n1","3"]]

Using jobs to select different arguments must also invalidate the action.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (source (copy $PWD/src))
  > (build (run sh -c : (if (= %{jobs} 1) one many)))
  > EOF
  $ target=$(get_build_pkg_dir foo)/target
  $ dune build -j 1 "$target"
  $ arguments
  [["one"]]
  $ dune build -j 2 "$target"
  $ arguments
  [["many"]]
