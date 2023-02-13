Testing the load_only_modules field of the coq.theory stanza. This field is used
to specify which modules should be avoided during compilation as they will be
loaded using a Load command.

Only valid module names that coq.theory knows about can be give to the
load_only_modules stanza.

  $ cat > dune << EOF
  > (coq.theory
  >  (name loadonly)
  >  (mode vo)
  >  (load_only_modules IDontExist)
  >  (package loadonly))
  > 
  > (include_subdirs qualified)
  > EOF

  $ dune build --display=short --debug-dependency-path
  File "dune", line 1, characters 0-92:
  1 | (coq.theory
  2 |  (name loadonly)
  3 |  (mode vo)
  4 |  (load_only_modules IDontExist)
  5 |  (package loadonly))
  Error: Field (load_only_modules) contains Coq modules that the coq.theory
  stanza doesn't know about.
  [1]

When modules are given to the load_only_modules field, they are not compiled and
only their sources are installed.

We have 3 cases to consider:

1. The user wrote `Load A.` in the file D.v.

In this case, coqdep will emit a dependency on A.v so we have to make sure that
A.v is not itself compiled.

2. The user wrote Load "B/C.v" in the file D.v.

Coqdep will emit the correct dependency (relative to the root of the theory) so
we need to make sure that this file is not compiled.

3. The user loads a file E.v that further requires a file F.v.

We need to make sure that the deps of E.v are injected into D.v.

  $ cat > dune << EOF
  > (coq.theory
  >  (name loadonly)
  >  (mode vo)
  >  (load_only_modules A B.C E)
  >  (package loadonly))
  > 
  > (include_subdirs qualified)
  > EOF

Now building D.vo should succeed. As we can see, cases 1, 2 and 3 have been
satisfied.

  $ dune build --display=short D.vo
        coqdep loadonly.theory.d
          coqc F.{glob,vo}
          coqc D.{glob,vo}

Next we build the install.

  $ dune build @install

We inspect the contents of the _build directory. Importantly observe that load
only modules A, B.C and E have not themselves been compiled.

  $ ls _build/default _build/default/B
  _build/default:
  A.v
  B
  D.glob
  D.v
  D.vo
  E.v
  F.glob
  F.v
  F.vo
  META.loadonly
  loadonly.dune-package
  loadonly.install
  loadonly.opam
  loadonly.theory.d
  
  _build/default/B:
  C.v

During installation, only the sources for load only modules A, B.C and E are
copied.

  $ dune install --prefix .
  Installing lib/loadonly/META
  Installing lib/loadonly/dune-package
  Installing lib/loadonly/opam
  Installing lib/coq/user-contrib/loadonly/A.v
  Installing lib/coq/user-contrib/loadonly/B/C.v
  Installing lib/coq/user-contrib/loadonly/D.v
  Installing lib/coq/user-contrib/loadonly/D.vo
  Installing lib/coq/user-contrib/loadonly/E.v
  Installing lib/coq/user-contrib/loadonly/F.v
  Installing lib/coq/user-contrib/loadonly/F.vo
