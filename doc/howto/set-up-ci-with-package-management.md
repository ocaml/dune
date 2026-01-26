# How to set up Continuous Integration with Package Management

For many projects published it can be useful to set up a [continuous
integration](https://en.wikipedia.org/wiki/Continuous_integration) (CI) system.
This allows to check whether the code builds on other computers, the tests pass
and helps to assess contributions from other people.

There are numerous CI systems available. In this document we will be using
GitHub Actions, as it is integrated in the commonly used GitHub platform and
free to use.

## What are Actions

Projects on GitHub can set up [Actions](https://docs.github.com/en/actions),
that is workflows defined in YAML files. These definitions are placed in the
`.github/workflows/` folder in the Git repository.

These workflows consist of the execution of a sequence of actions, like running
shell commands. It is also possible to use pre-defined actions or even
user-submitted functionality.

## `setup-dune`

[`setup-dune`](https://github.com/ocaml-dune/setup-dune) is one such custom
action maintained by the OCaml community to help with setting up builds with
Dune.

It installs Dune in the GitHub runner environment and will attempt to build the
source code using Dune package management.

::::{dropdown} `.github/workflow/ci.yaml`
:icon: file-code

:::{literalinclude} set-up-ci-with-package-management/ci.yaml
:language: yaml
:emphasize-lines: 15-16
:::

We define a job called `build`, which will be run on Ubuntu and macOS (a [list
of possible
options](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
can be found in the GitHub documentation).

This job first checks out the projects' source code using an action maintained
by GitHub and then uses the `setup-dune` action to install Dune on the system
and build the project.

`setup-dune` will enable package management by default and attempt to build the
project with the current nightly version of Dune.

Additional configuration options can be found in the [documentation of
`setup-dune`](https://github.com/ocaml-dune/setup-dune/?tab=readme-ov-file),
which includes picking a fixed version of Dune, picking a specific OCaml
compiler etc.

::::

This configuration file has to be added to the repository and pushed to GitHub.
Some configuration to enable actions in the repository might be required, check
out [the GitHub documentation on repository
settings](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/enabling-features-for-your-repository/managing-github-actions-settings-for-a-repository)
related to actions.

Once set up, this action will be run on the commit of every push to every
branch as well as on every pull request. The results of running the actions are
visible as a colored dot next to the relevant commits that were processed by
CI.

::::{seealso}

[GitHub Actions documentation](https://docs.github.com/en/actions) for
information about the configuration format and usual workflows.

[GitHub Actions marketplace](https://github.com/marketplace?type=actions) for a
list of published actions.
::::
