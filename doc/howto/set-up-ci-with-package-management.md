# How to set up Continuous Integration with Package Management

For many projects published it can be useful to set up a [continuous
integration](https://en.wikipedia.org/wiki/Continuous_integration) (CI) system.
This allows to check whether the code builds on other computers, the tests pass
and helps to assess contributions from other people.

There are numerous CI systems available. In this document we will be using
GitHub Actions, as it is integrated in GitHub and free to use.

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

It installs Dune and will attempt to build the source code.

::::{dropdown} `.github/workflow/ci.yaml`
:icon: file-code

:::{literalinclude} set-up-ci-with-package-management/ci.yaml
:language: yaml
:emphasize-lines: 15-18
:::

We define a job called `build`, which will be run on Ubuntu and macOS (a [list
of possible
options](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
can be found in the GitHub documentation).

This job first checks out the projects' source code using an action maintained
by GitHub and then uses the `setup-dune` action to install Dune on the system
and build the project.

It parametrizes the `setup-dune` action with `automagic` set to `true` which
instructs the action, amongst other things, to tell Dune to create a lock
directory. This lock directory will then be used to download, build and install
the project dependencies before building the project itself.

::::

This configuration file has to be added to the repository and pushed to GitHub.
Some configuration to enable actions in the repository mit be required, check
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
