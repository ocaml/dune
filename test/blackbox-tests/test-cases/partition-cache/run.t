The test checks the partition break-up, cleanliness, and inferred dependencies.

  $ sed -i -E 's/".*"/"Hello world"/' bar/b.ml

For the first run, there's no existing partition cache.

  $ dune build foo/main.exe --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition dirty (no record): <anonymous .>             (DIGEST)	 [deps: ]
  Partition dirty (no record): <anonymous foo>           (DIGEST)	 [deps: ]
  Partition dirty (no record): bar                       (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello world

The second run recalculates digests with dependencies saved from the first run.

  $ dune build foo/main.exe --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]
  Partition dirty: <anonymous .>             (DIGEST <- DIGEST)	 [deps: bar]
  Partition dirty: <anonymous foo>           (DIGEST <- DIGEST)	 [deps: bar, <anonymous .>, ...3 target deps]
  Partition dirty: bar                       (DIGEST <- DIGEST)	 [deps: , ...3 target deps]

  $ ./_build/default/foo/main.exe
  Hello world

The third run has complete cache, so doesn't recalculate any rules.

  $ dune build foo/main.exe --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <anonymous .>             (DIGEST)	 [deps: bar]
  Partition clean: <anonymous foo>           (DIGEST)	 [deps: bar, <anonymous .>, ...3 target deps]
  Partition clean: bar                       (DIGEST)	 [deps: , ...3 target deps]
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello world

  $ sed -i -E 's/".*"/"Hello again"/' bar/b.ml

Files in bar changed, should rebuild both projects.

  $ dune build foo/main.exe --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]
  Partition dirty: <anonymous .>             (DIGEST <- DIGEST)	 [deps: bar]
  Partition dirty: <anonymous foo>           (DIGEST <- DIGEST)	 [deps: bar, <anonymous .>, ...3 target deps]
  Partition dirty: bar                       (DIGEST <- DIGEST)	 [deps: , ...3 target deps]

  $ ./_build/default/foo/main.exe
  Hello again

Everything is clean again.

  $ dune build foo/main.exe --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <anonymous .>             (DIGEST)	 [deps: bar]
  Partition clean: <anonymous foo>           (DIGEST)	 [deps: bar, <anonymous .>, ...3 target deps]
  Partition clean: bar                       (DIGEST)	 [deps: , ...3 target deps]
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello again
