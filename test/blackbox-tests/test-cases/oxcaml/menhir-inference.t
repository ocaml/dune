Menhir inference supports library parameters, but prints invalid syntax for
an instantiated library's abstract result type.

  $ make_menhir_project 3.25 2.1
  $ echo '(using oxcaml 0.1)' >> dune-project
  $ mkdir param impl lib app
  $ echo '(library_parameter (name param))' >param/dune
  $ echo 'val answer : int' >param/param.mli
  $ echo '(library (name impl) (implements param))' >impl/dune
  $ echo 'let answer = 42' >impl/impl.ml
  $ cat >lib/dune <<'EOF'
  > (library (name lib) (parameters param))
  > (menhir (modules parser))
  > EOF
  $ cat >lib/lib.ml <<'EOF'
  > type t = int
  > let answer = Param.answer
  > EOF
  $ cat >lib/lib.mli <<'EOF'
  > type t
  > val answer : t
  > EOF
  $ cat >lib/parser.mly <<'EOF'
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Param.answer }
  > EOF

Inference must receive the library's parameters:

  $ dune build lib/parser.mli

An instance's abstract type must also be printable in the generated interface:

  $ cat >app/dune <<'EOF'
  > (library
  >  (name parser)
  >  (libraries (instantiate lib impl :as instance)))
  > (menhir (modules parser))
  > EOF
  $ cat >app/parser.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { Instance.answer }
  > EOF

  $ dune build %{cmi:app/Parser}
  File "app/parser.mli", line 13, characters 59-60:
  13 | val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lib[Param:Impl].t)
                                                                  ^
  Error: Syntax error
  [1]
