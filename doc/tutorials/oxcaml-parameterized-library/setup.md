# Setup The Project Structure

Parameterized libraries only works with a compiler supporting parameterized
libraries. Currently, only OxCaml supports this feature. Follow [the
instruction](https://oxcaml.org/get-oxcaml/) to get the OxCaml version of the
compiler.

Now, it is done, we will create a simple project structure we will make grow
during the tutorial

## Setting a Simple Project

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} setup/dune-project
:language: dune
:emphasize-lines: 4-4
:::

It is important to add the `oxcaml` extension. It allows dune to use the stanzas and fields specific to OxCaml

::::

TODO @maiste: add other files...
