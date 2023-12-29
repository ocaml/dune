# Dune Monorepo Benchmarking

Dune is continuously benchmarked for performance regressions. This includes micro-benchmarks run with `make bench` (refer to the [hacking guide](../hacking.rst)), and macro-benchmarks designed to test performance on large projects, known as "Monorepo benchmarks."

These benchmarks simulate real-world complexities by assembling a monorepository from a large subset of co-installable opam packages. They are essential for assessing the impact of changes on large codebases and providing a realistic performance profile. The metrics monitored include but are not limited to execution times and memory usage.

This document guides you on how to use and visualise the Monorepo benchmarks and details the assembly process of the monorepo.

*Note: For `current-bench` documentation (the tool running Dune benchmarks), visit [current-bench's repository](https://github.com/ocurrent/current-bench/tree/main/doc).*

## Accessing Benchmarks

Benchmark results are available [here](https://bench.ci.dev/ocaml/dune/branch/main?worker=fermat&image=bench%2Fmonorepo%2Fbench.Dockerfile).

[`current-bench`](https://github.com/ocurrent/current-bench/) is configured to automatically run Monorepo benchmarks on every new PR and commit to `main` in the `dune` repository. The dashboard defaults to the `main` branch, and you can adjust the time window in the UI's top right corner for your desired analysis duration.

### Benchmarks on Pull Requests

For every new PR, the benchmarks are automatically run. In the list of checks
on that PR's GitHub page, the benchmark runner adds a link to the dashboard
with the title `ocaml-benchmarks (bench/monorepo/bench.Dockerfile;fermat)`.
You can also use the [direct link to the
dashboard](https://bench.ci.dev/ocaml/dune/branch/main?worker=fermat&image=bench%2Fmonorepo%2Fbench.Dockerfile)
and find the PR number in the left sidebar.

The UI displays the PR's last commit hash used for the comparison.  It's a good
idea to verify that this is the commit you want to compare.

A table of comparison is displayed on the PR branch dashboard, which gives a
comparison of each metric's value on the PR's last commit vs the
`main` branch value. Plus, a delta percentage of the values that would
help you notice any significant changes to drill further into.

There's also a graph comparing metric values that could make it easier to
notice sharp changes in the metrics to further investigate and understand what
causes them.

For more information on how to use `current-bench`, refer to the [current-bench
User
Manual](https://github.com/ocurrent/current-bench/blob/main/doc/user_manual.md).

### Posting Results to the PR

You can opt-in to post benchmark results in the pull request directly. To do so, you can label the pull request with `notify-benchmark-results`. When a PR is tagged with this label, `current-bench` will post benchmark results after every commit.

See [this PR](https://github.com/ocaml/dune/pull/8596#issuecomment-1734763937) as an example.

## How to Run the Benchmarks Locally

To run the benchmarks locally, you need to set up a Docker environment for the
Monorepo benchmark, then generate and mount a `duniverse` directory.

1.  **Build the Docker Image**: From the root directory of your local `dune`
    repository, build the Docker image using the
    `bench/monorepo/bench.Dockerfile`

    ``` console
    $ docker build . -f bench/monorepo/bench.Dockerfile --tag=dune-monorepo-benchmark
    ```

2.  **Clone the Benchmark Repository**: Clone the `ocaml-monorepo-benchmark`
    repository to obtain necessary scripts and files.

    ``` console
    $ git clone https://github.com/ocaml-dune/ocaml-monorepo-benchmark.git
    $ cd ocaml-monorepo-benchmark
    ```

3.  **Generate the `duniverse` Directory**: Use the `generate-duniverse.sh`
    script to create the `duniverse` directory at a desired location (e.g.,
    `/tmp`).

    ``` console
    $ ./generate-duniverse.sh /tmp
    ```

    This will create a directory `/tmp/duniverse` containing all the Monorepo's packages.

4.  **Run the Benchmark**: Execute the Docker container, mounting the
    `duniverse` directory as a volume.

    ``` console
    $ docker run -it --volume=/tmp/duniverse:/home/opam/bench-dir/current-bench-data/duniverse dune-monorepo-benchmark bash --login -c 'make bench'
    ```

The results from the Monorepo benchmark are provided in a JSON format, which can
be parsed by [`current-bench`](https://bench.ci.dev).

## For Developers

Detailed documentation on how the Monorepo is constructed and updated is
available
[here](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/README.md).

### Descriptions of Existing Benchmarks

The benchmark runner runs two types of benchmarks: **one shot benchmarks** and
**watch mode benchmarks**.

#### One-Shot Benchmarks

These run `dune build <TARGET>` and record the time it takes for the command to
complete. Under normal operation, there are two one-shot benchmarks.

##### Build From Scratch

This builds the target assuming a clean state. As this
benchmark can take a long time (> 15 minutes) compared to the expected
noise in benchmark results, it's run only once.

##### Null Build
This builds the target assuming it's starting with the target already built.
This benchmark runs several times to reduce noise. The number of repetitions is
controlled by the `--num-short-job-repeats` argument.

#### Watch Mode Benchmarks

After running the one-shot benchmarks, the benchmark runner will start Dune in
watch mode. Once the initial build is complete, the watch mode benchmarks are
performed. Each watch mode benchmark makes a change to a file in the Monorepo
(which will trigger a rebuild) and measures how long it takes to rebuild the
target.

Each file is changed 3 times. The benchmark runner waits for a rebuild after each change:
 - A benign change is made to a source file.
 - A compile error is introduced into a source file.
 - The compile error is fixed.

There are 2 files which are changed in watch mode benchmarks. The intention was
to change a file in a library with few reverse dependencies and to change a
file in a library with many reverse dependencies:
 - The file `src/path.ml` in the library `file_path`, which has few reverse
   dependencies (within the monorepo)
 - The file `src/list.ml` in the library `base`, which has many reverse
   dependencies (within the monorepo)


### How to Add New Benchmarks

One-shot benchmarks are defined ad-hoc, so there's no structured way to add more,
but see the use of the function `measure_one_shot_build` in [this
file](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/dune-monorepo-benchmark-runner/src/main.ml)
to learn how one might add more one-shot benchmarks.

To add new watch mode benchmarks, edit the file
[dune-monorepo-benchmark-runner/src/watch_mode_scenarios.ml](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/dune-monorepo-benchmark-runner/src/watch_mode_scenarios.ml)
to add new entries to the list. The fields of list entries are documented
[here](https://github.com/ocaml-dune/ocaml-monorepo-benchmark/blob/main/dune-monorepo-benchmark-runner/src/watch_mode_file_to_change.mli).
Each entry specifies a file to change, some text in the file to be replaced and
two replacement text options -- something which compiles and something which
does not compile during the watch mode benchmark.

To have Dune start running the new benchmark in `current-bench`, do a new release
of the
[ocaml-monorepo-benchmark](https://github.com/ocaml-dune/ocaml-monorepo-benchmark)
by pushing a tag to the repo named the current date like `YYYY-MM-DD.N` where
`N` is the current day's release number, starting with 0.

Then update the `dune` repository, editing the
[bench/monorepo/bench.Dockerfile](https://github.com/ocaml/dune/blob/main/bench/monorepo/bench.Dockerfile)
file, setting the value of the `MONOREPO_BENCHMARK_TAG` variable to the tag
you just pushed.

Future PRs against Dune will now run the new benchmark.
