How to Configure Editors with Package Management
================================================

This document aims to help people set up their editors for use with
Dune package management. Different configurations are required than when using opam to manage dependencies.```

:::{note}
This list is not exhaustive. If you have a configuration for your editor that is not included here, we would welcome a [pull request to the Dune
repository](https://github.com/ocaml/dune/pulls) documenting the configuration.
:::

Emacs
-----

For Emacs, we recommend using `ocaml-eglot` mode with Dune package management. Follow the [instructions in the OCaml-eglot
repository](https://github.com/tarides/ocaml-eglot?tab=readme-ov-file#usage-with-dune-pkg).

Neovim
------

The instructions on how to configure `ocaml.nvim`, the OCaml plugin for Neovim
can be found [in the `ocaml.nvim`
repository](https://github.com/tarides/ocaml.nvim?tab=readme-ov-file#using-dune).


Visual Studio Code
------------------

The configuration for the VSCode OCaml Platform extension is [described in the
vscode-ocaml-platform
repository](https://github.com/ocamllabs/vscode-ocaml-platform?tab=readme-ov-file#dune-package-management-dpm).

More information
----------------

For general information on editor set up for OCaml (not including configuration specific to dune package management) check out [the community
documentation on OCaml.org](https://ocaml.org/docs/set-up-editor).
