Show regression from #14373 where local packages need explicit dependency
declarations when used with MDX.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name iter_mdx_regression))
  > EOF

  $ mkdir src

  $ cat >src/dune <<EOF
  > (library (public_name iter_mdx_regression))
  > EOF

  $ cat >src/iter_mdx_regression.ml <<EOF
  > let answer = 42
  > EOF

  $ cat >README.md <<'EOF'
  > ```ocaml
  > # #require "iter_mdx_regression";;
  > # Iter_mdx_regression.answer;;
  > - : int = 42
  > ```
  > EOF

This is the shape of the rule that used to work accidentally because
workspace packages were visible through the shared install area.

  $ cat >dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps
  >   (:dep README.md))
  >  (action
  >   (progn
  >    (run ocaml-mdx test %{dep})
  >    (diff? %{dep} %{dep}.corrected))))
  > EOF

  $ dune runtest
  File "README.md", line 1, characters 0-0:
  --- README.md
  +++ README.md.corrected
  @@ -1,5 +1,7 @@
   ```ocaml
   # #require "iter_mdx_regression";;
  +No such package: iter_mdx_regression
   # Iter_mdx_regression.answer;;
  -- : int = 42
  +Line 1, characters 1-20:
  +Error: Unbound module Iter_mdx_regression
   ```
  [1]

Declaring the package dependency gives the action an OCAMLPATH where
ocaml-mdx can find the local package.

  $ cat >dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps
  >   (package iter_mdx_regression)
  >   (:dep README.md))
  >  (action
  >   (progn
  >    (run ocaml-mdx test %{dep})
  >    (diff? %{dep} %{dep}.corrected))))
  > EOF

  $ dune runtest
