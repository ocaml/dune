# Monorepo Bench

Files for building a docker image for benchmarking dune building a large
monorepo composed of packages from the opam repo. Running `make bench` will
build the monorepo and print out a json object with benchmark results expected
to be consumed by current-bench.

The monorepo will be set up with opam-monorepo using the lockfile
[here](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/benchmark/monorepo-bench.opam.locked)
which is downloaded during `docker build`. Also during `docker build`,
a a library is created called `monorepo` with
[this](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/benchmark/dune)
dune file listing all the libraries to include in the build.
