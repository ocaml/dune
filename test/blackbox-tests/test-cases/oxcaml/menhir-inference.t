Menhir inference must retain library parameters and instantiated module names,
including in qualified module-group interfaces.

  $ make_menhir_project 3.25 2.1
  $ cat >> dune-project <<'EOF'
  > (using oxcaml 0.1)
  > EOF

  $ mkdir param impl lib
  $ cat > param/dune <<'EOF'
  > (library_parameter (name param))
  > EOF
  $ echo 'val answer : int' > param/param.mli
  $ cat > impl/dune <<'EOF'
  > (library (name impl) (implements param))
  > EOF
  $ echo 'let answer = 42' > impl/impl.ml

The parser inside the parameterized library uses the parameter directly.

  $ cat > lib/dune <<'EOF'
  > (library (name lib) (parameters param))
  > (menhir (modules parser))
  > EOF
  $ cat > lib/parser.mly <<'EOF'
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Param.answer }
  > EOF
  $ cat > lib/lib.mli <<'EOF'
  > type t
  > val answer : t
  > val to_int : t -> int
  > EOF
  $ cat > lib/lib.ml <<'EOF'
  > type t = int
  > let answer = Parser.main (fun _ -> Parser.EOF) (Lexing.from_string "")
  > let to_int x = x
  > EOF

The executable's parser uses an instance alias from its generated alias
module. Its inferred result must retain the instance's abstract type identity.

  $ mkdir -p app/group
  $ cat > app/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable
  >  (name main)
  >  (libraries (instantiate lib impl :as instance)))
  > EOF
  $ cat > app/group/dune <<'EOF'
  > (menhir (modules group))
  > EOF
  $ cat > app/group/group.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { Instance.answer }
  > EOF
  $ cat > app/main.ml <<'EOF'
  > let () =
  >   Group.main (fun _ -> Group.EOF) (Lexing.from_string "")
  >   |> Instance.to_int
  >   |> Printf.printf "%d\n"
  > EOF

  $ dune exec ./app/main.exe
  42
