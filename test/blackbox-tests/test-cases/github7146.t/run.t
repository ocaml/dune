The linker is not invoked from the directory containing the dune file with the
`(c_library_flags ...)` field that defines custom linker flags. This makes it
difficult to link against existing static libraries located in that directory.

Build a static c library
  $ gcc -c native.c
  $ ar rcs libnative.a native.o

The problem is demonstrated in the "problem" directory. The static library
created above is linked with a c wrapper that wraps the function in the static
library with ocaml types. A rule copies native_wrapper.c and libnative.a into
the problem directory, and the library should be located by the linker due to
the field:
(c_library_flags :standard -lnative -L.))

The linker is unable to find the library in this case. The following also
doesn't work:
(c_library_flags :standard -lnative -L%{project_root}/problem)

The error message will include the path to the linker which varies across
systems. Filter the linker path out of the error message.
  $ dune build problem 2>&1 | sed '/cannot find -lnative/ s/^[^:]*:/ld:/'
  File "problem/dune", line 1, characters 0-234:
   1 | (library
   2 |  (name foo_problem)
   3 |  (foreign_stubs
   4 |   (language c)
   5 |   (names native_wrapper))
   6 |  (no_dynlink)
   7 |  (foreign_archives native)
   8 |  (c_library_flags :standard -lnative -L.)
   9 | ;(c_library_flags :standard -lnative -L%{project_root}/problem)
  10 | )
  ld: cannot find -lnative: No such file or directory
  collect2: error: ld returned 1 exit status

The workaround is to use a rule to capture the path to the working directory
into a file, and then read the file inside the (c_library_flags ...) field:
  $ dune build workaround
