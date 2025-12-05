The parameterised libraries can themselves implement a parameter, leading to
this exponential sequence of instantiations:

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > (implicit_transitive_deps false)
  > EOF

A single parameter:

  $ mkdir x
  $ echo 'val v : string' > x/x.mli
  $ cat > x/dune <<EOF
  > (library_parameter (name x))
  > EOF

Various implementations of this parameter:

  $ echo 'let v = "X"' > x_impl.ml
  $ echo 'let v = "F(" ^ X.v ^ ")"' > f.ml
  $ echo 'let v = "G(" ^ F.v ^ ")"' > g.ml
  $ echo 'let v = "H(" ^ G.v ^ ")"' > h.ml
  $ echo 'let v = "I(" ^ H.v ^ ")"' > i.ml
  $ echo 'let v = "J(" ^ I.v ^ ")"' > j.ml

And a final binary:

  $ echo 'let () = print_endline J.v' > bin.ml

Each implementation of `x` is itself parameterised by `x`, with a dependency on
the previous implementation with `(lib lib)` which causes this exponential
doubling:

  $ cat > dune <<EOF
  > (library (name x_impl) (modules x_impl) (implements x))
  > (library (name f) (modules f) (parameters x) (implements x))
  > (library (name g) (modules g) (parameters x) (implements x) (libraries (instantiate f f)))
  > (library (name h) (modules h) (parameters x) (implements x) (libraries (instantiate g g)))
  > (library (name i) (modules i) (parameters x) (implements x) (libraries (instantiate h h)))
  > (library (name j) (modules j) (parameters x) (implements x) (libraries (instantiate i i)))
  > (executable (name bin) (modules bin) (libraries (instantiate j x_impl)))
  > EOF

The final result doubles for each layer:

  $ dune exec ./bin.exe
  J(I(H(G(F(F(G(F(F(H(G(F(F(G(F(F(I(H(G(F(F(G(F(F(H(G(F(F(G(F(F(X)))))))))))))))))))))))))))))))

Behind the scene, this test requires a correct encoding/decoding of
instantiated library names. For dune folders, the number of exclamation points
indicates the level of application nesting, i.e. `f!g!!x = f(g(x))` and `f!g!x
= f(g)(x)`.

The instantiated libraries are collected in .parameterised:

  $ ls _build/default/.parameterised/*
  f
  g
  h
  i
  j

With each lib folder containing the list of its instances:

  $ ls _build/default/.parameterised/*/g
  g!g!!h!!!i!!!!x_impl
  g!g!!h!!!x_impl
  g!g!!i!!!x_impl
  g!g!!x_impl
  g!h!!i!!!x_impl
  g!h!!x_impl
  g!i!!x_impl
  g!x_impl

For modules instantiated by the compiler, a dash is used:

  $ ls _build/default/.parameterised/*/f/f!f!!g!!!h!!!!i!!!!!x_impl/.instance.objs/native
  f-F--G---H----I-----X_impl.cmx
  f-F--G---H----I-----X_impl.o
  f__f__-F--G---H----I-----X_impl.cmx
  f__f__-F--G---H----I-----X_impl.o
