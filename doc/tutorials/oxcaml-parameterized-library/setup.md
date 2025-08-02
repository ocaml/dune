# Setting Up The Project Structure

Parameterized libraries only works with a compiler supporting parameterized
libraries. Currently, only OxCaml supports this feature. Follow [the
instruction](https://oxcaml.org/get-oxcaml/) to get the OxCaml version of the
compiler.

Now, it is done, we will create a simple project structure we will grow during
the tutorial.

## Setting a Simple Project

We create a `dune-project` file using the `3.20` version (or any higher
version) of the language:

:::{literalinclude} setup/dune-project
:language: dune
:::

:::{important}
It is mandatory to add the `oxcaml` extension. It allows dune to use the
stanzas and fields specific to OxCaml
:::

And... This is it! We have our basic structure to be able to build the project.

## Adding a basic test

Now we have our base, we want to add a simple test and make sure the test only
runs when the OxCaml compiler is available. We are going to use
{doc}`/reference/cram` to verify the program behaves as we expect.

First, we create a test directory to store our tests:

```{code-block} shell
mkdir test
```

Inside our `test/` directory, we add our `dune` with all the information
required to build the directory:

:::{literalinclude} setup/test/dune
:language: dune
:emphasize-lines: 3
:::

Let's analyze the content of the `dune` file. We declare add a
{doc}`/reference/dune/cram` stanza. Then we declare we want the stanza to apply
to the entire subtree of our `test/` directory. The highlighted part tells Dune
to only execute the cram test when OxCaml is supported. Again, this variable is
only available because we set the `oxcaml` extension.

Afterward, we create a `simple.t` file where we store our first test. The test
consists on building the project and make sure it compiles.

:::{literalinclude} setup/test/simple.t
:language: cram
:::

As the test succeeds, the test command should output nothing:
```{code-block} shell
dune test
echo "$?"
# 0
```

## Conclusion

In this part we have seen how to create a project where we active the support
for OxCaml. We have also learnt how to create a simple test suite, only run
when OxCaml is available.

Your directory should be in the following state after the tutorial.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} setup/dune-project
:language: dune
:::
::::

::::{dropdown} `test/dune`
:icon: file-code

:::{literalinclude} setup/test/dune
:language: dune
:::
::::

::::

::::{dropdown} `test/simple.t`
:icon: file-code

:::{literalinclude} setup/test/simple.t
:language: dune
:::
::::

::::


In the next part, we are going to explore how to write a `library_parameter`.
