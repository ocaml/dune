Test (excluded_modules) behavior in libraries

Version guard:

  $ mkdir guard
  $ cat > guard/dune-project <<'EOF'
  > (lang dune 3.21)
  > EOF

  $ cat > guard/dune <<'EOF'
  > (library
  >  (name x)
  >  (excluded_modules c))
  > EOF

  $ (cd guard && dune build 2>&1 | grep "only available since version 3.22")
  Error: 'excluded_modules' is only available since version 3.22 of the dune
  [1]

  $ rm -rf guard

  $ mkdir lib
  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (package
  >  (name p))
  > EOF

  $ cat > lib/dune <<'EOF'
  > (library
  >  (name x)
  >  (public_name p.x)
  >  (modules a b c)
  >  (excluded_modules c))
  > EOF

  $ cat > lib/a.ml <<'EOF'
  > let x = 42
  > EOF

  $ cat > lib/b.ml <<'EOF'
  > let y = C.z
  > EOF

  $ cat > lib/c.ml <<'EOF'
  > let z = A.x
  > EOF

Non-excluded modules cannot depend on excluded modules:

  $ dune build 2>&1 | grep "Unbound module"
  Error: Unbound module C
  [1]

Excluded modules are compiled, but not linked/installed:

  $ cat > lib/b.ml <<'EOF'
  > let y = A.x
  > EOF

  $ dune build @install

  $ grep "module C =" _build/default/lib/x.ml-gen
  [1]

  $ ocamlobjinfo _build/default/lib/x.cma | grep "Unit name:"
  Unit name: X
  Unit name: X__A
  Unit name: X__B

  $ grep "C\\.cm" _build/default/p.install
  [1]

Excluded modules are still compiled:

  $ cat > lib/c.ml <<'EOF'
  > let z = A.x
  > let bad = Does_not_exist.x
  > EOF

  $ dune build 2>&1 | grep "Unbound module"
  Error: Unbound module Does_not_exist
  [1]

  $ cat > lib/c.ml <<'EOF'
  > let z = A.x
  > EOF

  $ dune build

Dependent targets cannot see excluded modules:

  $ mkdir y
  $ cat > y/dune <<'EOF'
  > (library
  >  (name y)
  >  (libraries x))
  > EOF

  $ cat > y/y.ml <<'EOF'
  > let _ = X.C.z
  > EOF

  $ dune build y 2>&1 | grep "Unbound module"
  Error: Unbound module X.C
  [1]

  $ mkdir app
  $ cat > app/dune <<'EOF'
  > (executable
  >  (name app)
  >  (libraries x))
  > EOF

  $ cat > app/app.ml <<'EOF'
  > let () = ignore X.C.z
  > EOF

  $ dune build app 2>&1 | grep "Unbound module"
  Error: Unbound module X.C
  [1]

Same behavior for unwrapped libraries:

  $ mkdir ulib
  $ cat > ulib/dune <<'EOF'
  > (library
  >  (name u)
  >  (wrapped false)
  >  (modules a b c)
  >  (excluded_modules c))
  > EOF

  $ cat > ulib/a.ml <<'EOF'
  > let x = 7
  > EOF

  $ cat > ulib/b.ml <<'EOF'
  > let y = C.z
  > EOF

  $ cat > ulib/c.ml <<'EOF'
  > let z = A.x
  > EOF

  $ dune build ulib 2>&1 | grep "Unbound module"
  Error: Unbound module C
  [1]

  $ cat > ulib/b.ml <<'EOF'
  > let y = A.x
  > EOF

  $ dune build ulib

  $ mkdir uy
  $ cat > uy/dune <<'EOF'
  > (library
  >  (name uy)
  >  (libraries u))
  > EOF

  $ cat > uy/uy.ml <<'EOF'
  > let _ = C.z
  > EOF

  $ dune build uy 2>&1 | grep "Unbound module"
  Error: Unbound module C
  [1]

  $ mkdir uapp
  $ cat > uapp/dune <<'EOF'
  > (executable
  >  (name uapp)
  >  (libraries u))
  > EOF

  $ cat > uapp/uapp.ml <<'EOF'
  > let () = ignore C.z
  > EOF

  $ dune build uapp 2>&1 | grep "Unbound module"
  Error: Unbound module C
  [1]

(:include) is supported in excluded_modules:

  $ mkdir inclib
  $ mkdir cfg
  $ cat > cfg/dune <<'EOF'
  > (rule
  >  (target excluded.sexp)
  >  (action (write-file %{target} c)))
  > EOF

  $ cat > inclib/dune <<'EOF'
  > (library
  >  (name inclib)
  >  (modules a b c)
  >  (excluded_modules (:include ../cfg/excluded.sexp)))
  > EOF

  $ cat > inclib/a.ml <<'EOF'
  > let x = 1
  > EOF

  $ cat > inclib/b.ml <<'EOF'
  > let y = C.z
  > EOF

  $ cat > inclib/c.ml <<'EOF'
  > let z = A.x
  > EOF

  $ dune build inclib 2>&1 | grep "Unbound module"
  Error: Unbound module C
  [1]
