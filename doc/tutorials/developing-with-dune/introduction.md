Introduction
============

The goal of this first chapter is to get to a place where you have a working
Dune project with a skeleton of a calculator.

This is is a [tutorial](https://diataxis.fr/tutorials/): it is meant to be
followed in order, but you can stop at any point. You can also restart from any
chapter, using these sections that are present at the end of the previous
chapter.

::::{dropdown} Checkpoint
:icon: location
:open:

This will contain the project at the end of each chapter.
::::

## Installing Packages

First, you'll need to have a working Opam installation. This is described in
{doc}`/howto/install-dune`.

Then, create an empty directory somewhere, say `~/dune-calc`. In this tutorial,
we will only create files in this directory.

Let's first make sure you have a working opam installation. Run this command:

```sh
opam --version
```

It should display something like "2.2.0". Anything greater than 2.0.0 is fine.

:::{important}
When asked to type a command, you can click {octicon}`copy` to copy the full
command to your clipboard.

In this tutorial, all commands will be typed at the root of your project, like
`~/dune-calc`.
:::

Let's create a local switch: `cd` to this directory and run the following command.
This can take a few minutes.

```sh
opam switch create ./ 5.4.0
```

This command has created a directory named `_opam` in the current directory.
Now, let's install some packages by running:

```sh
opam install dune.3.15.3 menhir.20231231
```

You can confirm that `opam` is correctly setup by typing `dune --version`,
which should display 3.15.3. Otherwise, please refer to
{doc}`/howto/install-dune`.

:::{note}
The instructions use precise version numbers in `opam install` command. This is
to ensure that the error messages will exactly map what you're seeing, but
it is very likely to work with any version.
:::

## The Calculator Skeleton

Now that we have an opam switch and some packages installed, let's create the
various files.

For each file, click {octicon}`chevron-down` to reveal the file contents and
click {octicon}`copy` to copy the contents to your clipboard. Open `file.txt`
in a text editor and paste the contents there.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} introduction/dune-project
:language: dune
:::

This file contains metadata about the project:

- the version of the dune language we're using
- the extensions we're {doc}`using </reference/dune-project/using>`
- the {doc}`package </reference/dune-project/package>` we're defining

:::{seealso}
{doc}`/reference/dune-project/index`
  Reference documentation about `dune-project` files
:::
::::

::::{dropdown} `dune`
:icon: file-code

:::{literalinclude} introduction/dune
:language: dune
:::

This file contains a description of what's in our project:

- an {doc}`/reference/dune/executable` stanza defining our calculator binary
- an {doc}`/reference/dune/ocamllex` stanza, setting up rules to compile
  `lexer.mll` to a `Lexer` module
- a {doc}`/reference/dune/menhir` stanza, to similarly use `parser.mly` as a `Parser` module

::::

::::{dropdown} `parser.mly`
:icon: file-code

:::{literalinclude} introduction/parser.mly
:language: ocaml
:::

This contains definitions of our tokens and grammar rules, using
[Menhir](https://gallium.inria.fr/~fpottier/menhir/).
::::

::::{dropdown} `lexer.mll`
:icon: file-code

:::{literalinclude} introduction/lexer.mll
:language: ocaml
:::

This is our lexer, using [ocamllex](https://ocaml.org/manual/5.2/lexyacc.html).
::::

::::{dropdown} `ast.ml`
:icon: file-code

:::{literalinclude} introduction/ast.ml
:language: ocaml
:::

This contains a definition of the arithmetic expressions manipulated by the calculator.

:::{note}
This is in a separate file from `calc.ml` to avoid module cycles, since `Calc`
depends on `Parser`, which depends on the expression type.
:::
::::

:::{dropdown} `calc.ml`
:icon: file-code

:::{literalinclude} introduction/calc.ml
:language: ocaml
:::

This is the "business logic" of our app, in which we:
- display a prompt
- call the lexer and parser to get an expression
- evaluate the expression
- display the result
:::

At this stage, we have the skeleton of a calculator.

Run the following command to build and execute the calculator:

```sh
dune exec calc
```

You can enter additions, such as `1+2` followed by {kbd}`Enter`.
Exit with {kbd}`Ctrl+C`.

Initially, only addition is supported and anything else triggers an
exception terminating the execution.

Note that a `_build` directory is now present. This is where Dune will store
all compiled artifacts.

You can safely remove this directory - that's actually what the `dune clean`
command does. But that's not usually necessary since Dune will keep track of
dependencies and what is up to date.

The `_opam` directory is where your dependencies are located. It is managed by
opam. If it gets removed by accident or something is corrupted in there, it is
safe to remove it and recreate it by running `opam switch` and `opam install`
as described above.
