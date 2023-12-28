# Intended to be used from inside docker containers built from bench.Dockerfile
RUNNER = /home/user/monorepo-benchmark/dune-monorepo-benchmark-runner/_build/default/src/main.exe
DUNE_EXE_PATH = /home/user/dune/_build/default/bin/main.exe
BUILD_TARGET = ./monorepo_bench.exe
MONOREPO_PATH = /home/user/monorepo-benchmark/benchmark
USER_GROUP = user:users

# This benchmark is intended to run on current-bench where the duniverse
# directory is expected to exist at this path. See the readme for info on
# running this benchmark locally.
DUNIVERSE_MOUNT_POINT = /home/opam/bench-dir/current-bench-data/duniverse

# Copy the duniverse directory as root in case the regular user doesn't have
# permission to read from the mount point.
duniverse:
	sudo cp -a $(DUNIVERSE_MOUNT_POINT) .
	sudo chown -hR $(USER_GROUP) duniverse
	./apply-patches.sh

# Run the dune benchmark runner. To reduce noise, short benchmarks are run 5
# times each, and the max and min results of each benchmark are discarded. The
# remaining 3 results are presented to current-bench. Short benchmarks are all
# benchmarks except for the initial build from scratch.
bench: duniverse
	$(RUNNER) \
		--dune-exe-path=$(DUNE_EXE_PATH) \
		--build-target=$(BUILD_TARGET) \
		--monorepo-path=$(MONOREPO_PATH) \
		--print-dune-output \
		--num-short-job-repeats=5 \
		--remove-outliers=1

clean:
	dune clean

.PHONY: bench clean
