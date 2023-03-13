# Intended to be used from inside docker containers built from monorepo-bench.Dockerfile
RUNNER = _build/default/bench.exe
DUNE_TO_BENCHMARK = /home/user/dune/_build/default/bin/main.exe

$(RUNNER): dune bench.ml
	dune build $@ --release

bench: $(RUNNER)
	$< $(DUNE_TO_BENCHMARK) build -j auto

clean:
	dune clean

.PHONY: bench clean
