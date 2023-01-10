# Melange

All the required packages to work on Melange rules and tests are installed
automatically when calling `make dev-deps`.

## Build benchmarks

Some build time benchmarks are executed on CI (see
[`melange-bench.yml`](https://github.com/ocaml/dune/blob/main/.github/workflows/melange-bench.yml)).

The benchmarks run for every commit on `main` branch, and the results are
automatically appended to a public GitHub pages site, which can be seen in
https://ocaml.github.io/dune/dev/bench/.
