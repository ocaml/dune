# Intended to be used from inside docker containers built from bench.Dockerfile
RUNNER = /home/user/monorepo-benchmark/dune-benchmark-runner/_build/default/src/main.exe
DUNE_EXE_PATH = /home/user/dune/_build/default/bin/main.exe
BUILD_TARGET = ./monorepo_bench.exe
MONOREPO_PATH = /home/user/monorepo-benchmark/benchmark

bench:
	$(RUNNER) --dune-exe-path=$(DUNE_EXE_PATH) --build-target=$(BUILD_TARGET) --monorepo-path=$(MONOREPO_PATH) --print-dune-output

clean:
	dune clean

.PHONY: bench clean
