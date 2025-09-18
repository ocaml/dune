# Create a Library Parameter

In this section, we are going to learn how to write a
{doc}`/reference/dune/library_parameter`

## Declaring a parameter

We can either declare our parameter private or public in Dune. In our case, we
are going to declare it as public. Indeed, Dune doesn't build unecessary
targets. It means it won't build anything that is not needed. In our case, it
would mean our parameter wouldn't get build until we use it. Making it public
informs Dune it is mandatory to build the target.

To build our parameter we are going to use the
{doc}`/reference/dune/library_parameter` stanza.

First, we need to create the directory where we are going to host our
parameter. At the root of our project, we create a `lib/` directory where we
are going to host the code. Inside the `lib/` directory, we create a `param/`
directory to host our parameter. We do this in one command to save
instructions.

```{code-block} shell
mkdir -p lib/param
```

In `lib/param/`, we create a new `dune` file:

:::{literalinclude} parameter/lib/param/dune
:language: dune
:::
::::

The `library_parameter` stanza takes care of the parameter generation under the
hood. It calls the OxCaml compiler with the correct options and flags.

Still in `lib/param/`, we create the interface for our future libraries in `param.mli`:

:::{literalinclude} parameter/lib/param/param.mli
:language: ocaml
:::
::::

This interface is what our implementation library will need to provide and the
interface of our future parameterized library. 

## Conclusion

We have learned how to declare a parameter using Dune with its interface.

Your directory should now contains two additional files:

::::{dropdown} `lib/param/dune`
:icon: file-code

:::{literalinclude} parameter/lib/param/dune
:language: dune
:::
::::

::::{dropdown} `lib/param/param.mli`
:icon: file-code

:::{literalinclude} parameter/lib/param/param.mli
:language: ocaml
:::
::::

::::

The file from the previous should be in the following state:

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} parameter/dune-project
:language: dune
:::
::::

::::{dropdown} `test/dune`
:icon: file-code

:::{literalinclude} parameter/test/dune
:language: dune
:::
::::

::::

::::{dropdown} `test/simple.t`
:icon: file-code

:::{literalinclude} parameter/test/simple.t
:language: dune
:::
::::

In the next part, we are going to learn how to write the implementation of our
parameter.

