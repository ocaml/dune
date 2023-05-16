This project is called `hello_world`. It defines one library called
`hello_world` and one executable called `hello_world`.

The library is defined in `lib` and the executable in `bin`. It also defines a
test in `test`.

At the toplevel of the project, there is a `hello_world.opam` file. This file
is required so that Dune knows that this is the `hello_world` project.

To build everything that is meant to be installed in this project, type:

    $ dune build @install

To run the tests, type:

    $ dune runtest
