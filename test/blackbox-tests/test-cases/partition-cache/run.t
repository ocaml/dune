The test checks the partition break-up, cleanliness, and inferred dependencies.

  $ sed -i -E 's/".*"/"Hello world"/' bar/b.ml

For the first run, there's no existing partition cache.

  $ dune build foo/main.exe --use-partitions --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Info: creating file dune-project with this contents: (lang dune 1.2)
  Partition dirty (no record): bar                       (DIGEST)	 [deps: ]
  Partition dirty (no record): <dir: foo>                (DIGEST)	 [deps: ]
  Partition dirty (no record): <dir: foo/.main.eobjs>    (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition dirty (no record): <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello world

The second run recalculates digests with dependencies saved from the first run.

  $ dune build foo/main.exe --use-partitions --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]
  Partition dirty: bar                       (DIGEST <- DIGEST)	 [deps: , ...3 target deps]
  Partition dirty: <dir: foo>                (DIGEST <- DIGEST)	 [deps: bar, <dir: foo/.main.eobjs>, ...1 target deps]
  Partition dirty: <dir: foo/.main.eobjs>    (DIGEST <- DIGEST)	 [deps: bar, <dir: foo>, ...3 target deps]

  $ ./_build/default/foo/main.exe
  Hello world

The third run has complete cache, so doesn't recalculate any rules.

  $ dune build foo/main.exe --use-partitions --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: bar                       (DIGEST)	 [deps: , ...3 target deps]
  Partition clean: <dir: foo>                (DIGEST)	 [deps: bar, <dir: foo/.main.eobjs>, ...1 target deps]
  Partition clean: <dir: foo/.main.eobjs>    (DIGEST)	 [deps: bar, <dir: foo>, ...3 target deps]
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello world

  $ sed -i -E 's/".*"/"Hello again"/' bar/b.ml

Files in bar changed, should rebuild both projects.

  $ dune build foo/main.exe --use-partitions --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]
  Partition dirty: bar                       (DIGEST <- DIGEST)	 [deps: , ...3 target deps]
  Partition dirty: <dir: foo>                (DIGEST <- DIGEST)	 [deps: bar, <dir: foo/.main.eobjs>, ...1 target deps]
  Partition dirty: <dir: foo/.main.eobjs>    (DIGEST <- DIGEST)	 [deps: bar, <dir: foo>, ...3 target deps]

  $ ./_build/default/foo/main.exe
  Hello again

Everything is clean again.

  $ dune build foo/main.exe --use-partitions --debug-partition-cache 2>&1 | sed -E 's/\w{32}/DIGEST/g' | sed -E 's/<target: \/.*\/([^\]*)>/\<target: ...\/\1\>/g' | sort
  Partition clean: bar                       (DIGEST)	 [deps: , ...3 target deps]
  Partition clean: <dir: foo>                (DIGEST)	 [deps: bar, <dir: foo/.main.eobjs>, ...1 target deps]
  Partition clean: <dir: foo/.main.eobjs>    (DIGEST)	 [deps: bar, <dir: foo>, ...3 target deps]
  Partition clean: <target: .../ocamlc.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamldep.opt> (DIGEST)	 [deps: ]
  Partition clean: <target: .../ocamlopt.opt> (DIGEST)	 [deps: ]

  $ ./_build/default/foo/main.exe
  Hello again
