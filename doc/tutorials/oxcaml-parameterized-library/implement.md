# Implement a Library Parameter

Now we have our parameter, we have our interface. In this section, we will
focus on how to write a library that implements our parameter.

## Structure Our Project

As we did for the `parameter`, we are going to add a new directory in our
structure under `lib/`. We create a directory `impl/` under `lib/`. At the root
of the directory, we can run:

```{code-block} shell
mkdir -p lib/impl
```

## Creating an Implementation

As for the parameter, we start with our `lib/impl/dune`

:::{literalinclude} implement/lib/impl/dune
:language: dune
:::

To create an implementation, we write two files:
- The first one is `lib/impl/impl.ml`. It contains the implementation.
- The second one is `lib/impl/impl.mli`. It contains the interface of the implementation.


We define an additional `create` function to send our element in the abstract
type. Our project now implements a parameter.

:::{literalinclude} implement/lib/impl/impl.mli
:language: ocaml
:::

To have an abstract interface, we define an `mli` file.

:::{literalinclude} implement/lib/impl/impl.ml
:language: ocaml
:::


## Conclusion

In this section you have learned to implement a parameter. We have added the
following files:

::::{dropdown} `lib/impl/dune`
:icon: file-code

:::{literalinclude} implement/lib/impl/dune
:language: dune
:::
::::

::::{dropdown} `lib/impl/impl.ml`
:icon: file-code

:::{literalinclude} implement/lib/impl/impl.ml
:language: ocaml
:::
::::

::::{dropdown} `lib/impl/impl.mli`
:icon: file-code

:::{literalinclude} implement/lib/impl/impl.mli
:language: ocaml
:::
::::

You should also have the previous files:

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} implement/dune-project
:language: dune
:::
::::

::::{dropdown} `test/dune`
:icon: file-code

:::{literalinclude} implement/test/dune
:language: dune
:::
::::

::::

::::{dropdown} `test/simple.t`
:icon: file-code

:::{literalinclude} implement/test/simple.t
:language: dune
:::
::::

::::{dropdown} `lib/param/dune`
:icon: file-code

:::{literalinclude} implement/lib/param/dune
:language: dune
:::
::::

::::{dropdown} `lib/param/param.mli`
:icon: file-code

:::{literalinclude} implement/lib/param/param.mli
:language: ocaml
:::
::::

::::

In the next section, we will see how we write a library parameterized by our
parameter.
