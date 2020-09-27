A library can be shadowed by an internal module name:

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ mkdir lib0 lib1 lib2

  $ cat >lib0/dune <<EOF
  > (library
  >  (name lib0))
  > EOF
  $ cat >lib0/lib0.ml <<EOF
  > let greeting_from_lib0 = "Hello World"
  > EOF

  $ cat >lib1/dune <<EOF
  > (library
  >  (name lib1))
  > EOF
  $ cat >lib1/lib1.ml <<EOF
  > let greeting = "Hello World"
  > EOF

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries lib1)
  >  (name lib2))
  > EOF

Now we shadow lib1:
  $ touch lib2/lib1.ml
  $ cat >lib2/lib2.ml <<EOF
  > print_endline Lib1.greeting
  > EOF

  $ dune build @all
  File "lib2/lib2.ml", line 1, characters 14-27:
  1 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: Unbound value Lib1.greeting
  [1]

"identity" renaming does not change the name precedence:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries (rename lib1 -> lib1))
  >  (name lib2))
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > print_endline Lib1.greeting
  > EOF
  $ dune build @all
  File "lib2/lib2.ml", line 1, characters 14-27:
  1 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: Unbound value Lib1.greeting
  [1]

We can use the rename dependency type to use lib1 with a different name:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries (rename lib1 -> lib1_unshadow))
  >  (name lib2))
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > print_endline Lib1_unshadow.greeting
  > EOF
  $ dune build @all

The same for executables:

  $ mkdir exe
  $ cat >exe/dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries (rename lib1 -> lib1_unshadow)))
  > EOF
  $ touch exe/lib1.ml
  $ cat >exe/foo.ml <<EOF
  > print_endline Lib1_unshadow.greeting
  > EOF
  $ dune exec ./exe/foo.exe
  Hello World

This works for single module executables:

  $ rm exe/lib1.ml
  $ dune exec ./exe/foo.exe
  Hello World

And for single module libs:
  $ rm lib2/lib1.ml
  $ dune build @lib2/all

This mode is disabled for unwrapped libraries

  $ mkdir unwrapped
  $ cat >unwrapped/dune <<EOF
  > (library
  >  (libraries (rename lib1 -> lib1_unshadow))
  >  (wrapped false)
  >  (name unwrapped_lib))
  > EOF
  $ dune build @unwrapped/all
  File "unwrapped/dune", line 2, characters 20-24:
  2 |  (libraries (rename lib1 -> lib1_unshadow))
                          ^^^^
  Error: rename may not be used in unwrapped libraries
  [1]

  $ rm -r unwrapped

The renamed library can not be used by its original name:

  $ cat >lib2/lib2.ml <<EOF
  > module Lib1_empty = struct include Lib1 end;;
  > print_endline Lib1.greeting
  > EOF

  $ dune build @lib2/all
  File "lib2/lib2.ml", line 2, characters 14-27:
  2 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: Unbound value Lib1.greeting
  [1]

# CR-someday aalekseyev: I would rather [Lib1_empty] definition
# above was rejected.
#
# In jenga we achieve this by defining the lib alias like this:
#
# module Lib = Module_that_does_not_exist
#
# which has its own downsides, but we thought it was a bit better than
# an empty module. I think we also considered making it a functor
# that can't be applied, something like:
#
# module type Abstract
# module Lib1(M : Abstract) = struct end
#
# but that can be pretty confusing too.

Implementation detail: the generated renaming module.

  $ cat _build/default/lib2/lib2__.ml-gen
  module Lib1_unshadow = Lib1
  
  module Lib1 = struct let this_module_is_shadowed = () end

Multiple renamings to the same name:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries
  >    (rename lib0 -> lib)
  >    (rename lib1 -> lib)
  >   )
  >  (name lib2))
  > EOF

  $ dune build @lib2/all
  File "lib2/lib2__.ml-gen", line 3, characters 0-17:
  3 | module Lib = Lib1
      ^^^^^^^^^^^^^^^^^
  Error: Multiple definition of the module name Lib.
         Names must be unique in a given structure or signature.
  [1]

# CR aalekseyev: the error above should probably be caught earlier

Complicated library renamings where the act of renaming shadows another library:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries
  >    (rename lib0 -> lib1)
  >    (rename lib1 -> lib0)
  >   )
  >  (name lib2))
  > EOF

  $ cat >lib2/lib2.ml <<EOF
  > let greeting_from_lib1 = Lib0.greeting
  > let greeting_from_lib0 = Lib1.greeting_from_lib0
  > EOF

  $ cat >lib2/m.ml <<EOF
  > let x = 8
  > EOF

  $ dune build @lib2/all
  File "lib2/lib2__.ml-gen", line 5, characters 0-57:
  5 | module Lib0 = struct let this_module_is_shadowed = () end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Multiple definition of the module name Lib0.
         Names must be unique in a given structure or signature.
  [1]

# CR aalekseyev: the above should probably work?
# Or at least if it fails it should fail with a better error.
#
# Probably the way to make it work is to generate something like this:
#
# module Root____ = struct
#   module Lib0 = Lib0
#   module Lib1 = Lib1
# end
# module Lib0 = Root____.Lib1
# module Lib1 = Root____.Lib0
#
# (at this point, one has to wonder if Root____ is, perhaps, all we needed, after all)

Implementation detail, the generated renaming module:

  $ cat _build/default/lib2/lib2__.ml-gen
  module Lib0 = Lib1
  
  module Lib1 = Lib0
  
  module Lib0 = struct let this_module_is_shadowed = () end
  
  module Lib1 = struct let this_module_is_shadowed = () end
  
  (** @canonical Lib2.M *)
  module M = Lib2__M

# CR aalekseyev: should we also add a @canonical doc comment to the library renamings?
# I don't know what uses it, but if it's needed on M it's probably needed on Lib1 too.
