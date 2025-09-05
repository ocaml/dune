How to Override the Default C Entrypoint With C Stubs
-----------------------------------------------------

In some cases, it may be necessary to override the default C entry point of an
OCaml program. For example, this is the case if you want to let your program
handle argument wildcards expansion on Windows.

Let's consider a trivial "Hello world" program contained in a ``hello.ml``
file:

.. code:: ocaml

    let () = print_endline "Hello, world!"

The default C entry point is a ``main`` function, originally defined in
`runtime/main.c <https://github.com/ocaml/ocaml/blob/trunk/runtime/main.c>`_. It
can be overridden by defining a ``main`` function that will at some point call
the OCaml runtime. Let's write such a minimal example in a ``main.c`` file:

.. code:: C

    #include <stdio.h>

    #define CAML_INTERNALS
    #include "caml/misc.h"
    #include "caml/mlvalues.h"
    #include "caml/sys.h"
    #include "caml/callback.h"

    /* This is the new entry point */
    int main(int argc, char_os **argv)
    {
        /* Here, we just print a statement */
        printf("Doing stuff before calling the OCaml runtime\n");

        /* Before calling the OCaml runtime */
        caml_main(argv);
        caml_do_exit(0);
        return 0;
    }

The :doc:`foreign_stubs </reference/foreign-stubs>` stanza can be leveraged to
compile and link our OCaml program with the new C entry point defined in
``main.c``:

.. code:: dune

    (executable
     (name hello)
     (foreign_stubs
      (language c)
      (names main)))

With this ``dune`` file, the whole program can be compiled by merely calling
``dune build``. When run, the output shows that it calls the custom entry point
we defined:

.. code:: shell-session

    $ dune build
    $ _build/default/hello.exe
    Doing stuff before calling the OCaml runtime
    Hello, world!
