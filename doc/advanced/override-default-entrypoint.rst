Overriding the default entry point
----------------------------------

In some cases, it may be necessary to override the default main entry point of
an Ocaml program. For example, this is the case if you want to let your program
handle argument wildcards expansion on Windows.

Fortunately, the ``foreign_stubs`` stanza can be leveraged for that usage.

Let's consider a ``hello.ml`` file and the following ``main.c`` file that
redefines the entry point:

.. code:: C

    #include <stdio.h>

    #include "caml/mlvalues.h"
    #include "caml/callback.h"

    CAMLextern void caml_do_exit (int);

    int main_os(int argc, char_os **argv)
    {
        printf("Do stuff before calling the actual Ocaml program\n");

        caml_main(argv);
        caml_do_exit(0);
        return 0;
    }


Then the following will compile the ``hello.exe`` binary and make it use the entry
point defined in ``main.c``:

.. code:: dune

    (executable
     (name hello)
     (foreign_stubs
      (language c)
      (names main)))
