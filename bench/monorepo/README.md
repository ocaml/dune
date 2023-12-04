# Monorepo Bench

Files for building a docker image for benchmarking dune building a large
monorepo composed of packages from the opam repo. Running `make bench` will
build the monorepo and print out a json object with benchmark results expected
to be consumed by current-bench.

During `docker build`, an executable dune project is created called
`monorepo_bench` with
[this](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/benchmark/dune)
dune file listing all the libraries to include in the build.
This project is assembled with opam-monorepo, but we avoid running `opam
monorepo pull` while building the docker image as that command can fail if any
one of the thousands of dependencies are unavailable (which can happen for a
number of reasons). Instead, the duniverse directory must be provided as a
docker volume when running the container. For compatibility with the
current-bench environment, the duniverse directory must be located at:
```
/home/opam/bench-dir/current-bench-data/duniverse
```
The duniverse directory can be created with [this
script](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/generate-duniverse.sh).

The benchmark runner is a program which invokes dune in a number of
benchmarking scenarios and prints out the results in a format understood by
[current-bench](https://github.com/ocurrent/current-bench). The source code for
the benchmark runner is
[here](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/tree/main/dune-benchmark-runner).

## Running the benchmark locally

Build the benchmark docker image. Run this from the top level of the dune repo:
```
$ docker build . -f bench/monorepo/bench.Dockerfile --tag dune-monorepo-benchmark
```

Get the script that generates the duniverse dir:
```
$ git clone https://github.com/ocaml-dune/ocaml-monorepo-benchmark.git
$ cd ocaml-monorepo-benchmark
```

Run the script to generate the duniverse dir. This will generate it at
/tmp/duniverse but you can put it anywhere.
```
$ ./generate-duniverse.sh /tmp
```

Run the benchmark. The `/tmp/duniverse` in the argument to `--volume`
corresponds to the duniverse directory generated above.
```
$ docker run --rm -it --volume=/tmp/duniverse:/home/opam/bench-dir/current-bench-data/duniverse dune-monorepo-benchmark bash --login -c 'make bench'
```
