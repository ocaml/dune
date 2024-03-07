This test characterizes how the ctypes support deals with directories, in
particular when relative paths are passed as `-I`.

This test checks that relative paths are relative to the directory where rules
are defined, to make sure that vendoring works.

To test that, we create a binding to `BUFSIZ` and `fopen`, both in the C
library. These are provided by `stdio.h`, but instead of using this header,
we're proxying it through a `lib.h` which should be in the include path for
compilation to work.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using ctypes 0.3)
  > EOF

  $ mkdir lib

  $ cat > lib/dune << EOF
  > (executable
  >  (name e)
  >  (flags :standard -w -27)
  >  (ctypes
  >   (build_flags_resolver
  >    (vendored (c_flags :standard -I .)))
  >   (external_library_name l)
  >   (headers (include lib.h))
  >   (deps lib.h)
  >   (type_description
  >    (functor f)
  >    (instance types))
  >   (function_description
  >    (functor f)
  >    (instance functions))
  >   (generated_entry_point entry)))
  > EOF

  $ cat > lib/lib.h << EOF
  > #include <stdio.h>
  > EOF

  $ cat > lib/e.ml << EOF
  > let () = Printf.printf "%d\n" Entry.Types.bufsiz
  > let (_:char Ctypes.ptr -> char Ctypes.ptr -> Entry.Functions.file) = Entry.Functions.fopen
  > EOF

  $ cat > lib/f.ml << EOF
  > module Types(F:Ctypes.TYPE) = struct
  >   open F
  >   let bufsiz = constant "BUFSIZ" int
  > end
  > 
  > module Functions(F:Ctypes.FOREIGN) = struct
  >   open Ctypes
  >   open F
  >   type file = unit ptr
  >   let file : file typ = ptr void
  >   let fopen = foreign "fopen" (ptr char @-> ptr char @-> returning file)
  > end
  > EOF

  $ dune build
