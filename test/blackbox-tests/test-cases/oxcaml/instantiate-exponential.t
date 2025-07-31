The parameterized libraries can themselves implement a parameter, leading to
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

Each implementation of `x` is itself parameterized by `x`, with a dependency on
the previous implementation with `(lib lib)` which causes this exponential
doubling:

  $ cat > dune <<EOF
  > (library (name x_impl) (modules x_impl) (implements x))
  > (library (name f) (modules f) (parameters x) (implements x))
  > (library (name g) (modules g) (parameters x) (implements x) (libraries (f f)))
  > (library (name h) (modules h) (parameters x) (implements x) (libraries (g g)))
  > (library (name i) (modules i) (parameters x) (implements x) (libraries (h h)))
  > (library (name j) (modules j) (parameters x) (implements x) (libraries (i i)))
  > (executable (name bin) (modules bin) (libraries (j x_impl)))
  > EOF

The final result doubles for each layer:

  $ dune exec ./bin.exe
  J(I(H(G(F(F(G(F(F(H(G(F(F(G(F(F(I(H(G(F(F(G(F(F(H(G(F(F(G(F(F(X)))))))))))))))))))))))))))))))

Behind the scene, this test requires a correct encoding/decoding of
instantiated library names. For dune folders, the number of exclamation points
indicates the level of application nesting, i.e. `f!g!!x = f(g(x))` and `f!g!x
= f(g)(x)`.

  $ ls _build/default/.parameterized
  f!f!!g!!!h!!!!i!!!!!x_impl
  f!f!!g!!!h!!!!x_impl
  f!f!!g!!!i!!!!x_impl
  f!f!!g!!!x_impl
  f!f!!h!!!i!!!!x_impl
  f!f!!h!!!x_impl
  f!f!!i!!!x_impl
  f!f!!x_impl
  f!g!!h!!!i!!!!x_impl
  f!g!!h!!!x_impl
  f!g!!i!!!x_impl
  f!g!!x_impl
  f!h!!i!!!x_impl
  f!h!!x_impl
  f!i!!x_impl
  f!x_impl
  g!g!!h!!!i!!!!x_impl
  g!g!!h!!!x_impl
  g!g!!i!!!x_impl
  g!g!!x_impl
  g!h!!i!!!x_impl
  g!h!!x_impl
  g!i!!x_impl
  g!x_impl
  h!h!!i!!!x_impl
  h!h!!x_impl
  h!i!!x_impl
  h!x_impl
  i!i!!x_impl
  i!x_impl
  j!x_impl

For modules instantiated by the compiler, a dash is used:

  $ ls _build/default/.parameterized/f!f!!g!!!h!!!!i!!!!!x_impl/.instance.objs/native
  f-F--G---H----I-----X_impl.cmx
  f-F--G---H----I-----X_impl.o
  f__f__-F--G---H----I-----X_impl.cmx
  f__f__-F--G---H----I-----X_impl.o
