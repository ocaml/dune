Test the ability of `(modules_without_implementation)` to contain dynamic
forms such as `(:include)` and variables such as `"%{read-lines:}"`.

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

As we will see later in the test, it is imperative that build dependencies
needed to evaluate the `(modules)` field not live in the same directory as the
containing stanza. We will put them in a subdirectory:

  $ mkdir -p gen

We define a rule that creates a file (in sexp syntax, to be passed to
`(:include)`) containing a single name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo mod)))
  > EOF

The unit `mod.mli` is present in the working tree, `lib.ml` uses it:

  $ cat >mod.mli <<EOF
  > type t = | A | B
  > EOF

  $ cat >main.ml <<EOF
  > let f (x: Mod.t) = match x with A -> "a" | B -> "b"
  > let () = print_endline (f A)
  > EOF

We declare a `library` where the list of modules_without_implementation is read
from the (generated)

file `gen/lst`:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes byte)
  >  (modules main (:include gen/lst))
  >  (modules_without_implementation (:include gen/lst)))
  > EOF

Let's check that it fails in the current version of Dune:

  $ dune exec ./main.exe
  File "dune", line 5, characters 1-52:
  5 |  (modules_without_implementation (:include gen/lst)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: the ability to specify non-constant module lists is only available
  since version 3.13 of the dune language. Please update your dune-project file
  to have (lang dune 3.13).
  [1]

Update the version...

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

... and it works!

  $ dune exec ./main.exe
  a

Let's do some examples using libraries:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules (:include gen/lst))
  >  (modules_without_implementation (:include gen/lst)))
  > EOF
  $ dune build lib.cma

We can also use special forms such as `%{read-lines:}`:

  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (modes byte)
  >  (modules %{read-lines:gen/lst})
  >  (modules_without_implementation %{read-lines:gen/lst}))
  > EOF

