The project has an `.mld` file that needs to be fixed. At first, we determine
that that file is not being picked up:

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using mdx 0.3)
  > EOF
  $ cat > dune <<EOF
  > (mdx)
  > EOF
  > cat > needs-fixes.md <<'EOF'
  > This is a sample md file. It has some code that is invalid.
  > 
  > ```ocaml
  >    # List.map (fun x -> x * x) [(1 + 9); 2; 3; 4];;
  >    - : int list = [1; 2; 3; 8]
  > ```
  > 
  > A run of MDX should output a fixed version.
  > EOF
  $ cat > needs-fixes.mld <<EOF
  > This is a sample mld file. It has some code that is invalid.
  >  
  > {[
  >   # List.map (fun x -> x * x) [(1 + 9); 2; 3; 4];;
  >   - : int list = [1; 2; 3; 8]
  > ]}
  > 
  > A run of MDX should output a fixed version.
  > EOF
  $ dune runtest
  File "needs-fixes.md", line 1, characters 0-0:
  Error: Files _build/default/needs-fixes.md and
  _build/default/.mdx/needs-fixes.md.corrected differ.
  [1]

It did pick up the error in the `.md` file, so let's promote the fix.

  $ dune promote
  Promoting _build/default/.mdx/needs-fixes.md.corrected to needs-fixes.md.
  $ dune runtest

So the tests pass now, but it only uses the default `.md` glob for files to
process.

Let's also run the test for `.mld` files. These need to be manually passed, at
least as of mdx stanza version 0.3 and require MDX 2.3.0 at minimum.

  $ cat > dune <<EOF
  > (mdx
  >   (files needs-fixes.md needs-fixes.mld))
  > EOF
  $ dune runtest
  File "needs-fixes.mld", line 1, characters 0-0:
  Error: Files _build/default/needs-fixes.mld and
  _build/default/.mdx/needs-fixes.mld.corrected differ.
  [1]

The error in the `.mld` file was found, promoting should work and re-running
the test should succeed this time.

  $ dune promote
  Promoting _build/default/.mdx/needs-fixes.mld.corrected to needs-fixes.mld.
  $ dune runtest

The 0.4 version of the stanza adds support for `.mld` files by default, so bump
the stanza version.

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using mdx 0.4)
  > EOF
  $ cat > dune <<EOF
  > (mdx)
  > EOF
  $ cat > needs-fixes.mld <<EOF
  > This is a sample mld file. It has some code that is invalid.
  >  
  > {[
  >   # List.map (fun x -> x * x) [(1 + 9); 2; 3; 4];;
  >   - : int list = [1; 2; 3; 8]
  > ]}
  > 
  > A run of MDX should output a fixed version.
  > EOF

0.4 is only supported since dune-lang 3.8, so attempting to use it should fail:

  $ dune runtest
  File "dune-project", line 2, characters 11-14:
  2 | (using mdx 0.4)
                 ^^^
  Error: Version 0.4 of mdx extension to verify code blocks in .md files is not
  supported until version 3.8 of the dune language.
  Supported versions of this extension in version 3.7 of the dune language:
  - 0.1 to 0.3
  [1]

Updating the dune-lang should make the test run.

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using mdx 0.4)
  > EOF
  $ dune runtest
  File "needs-fixes.mld", line 1, characters 0-0:
  Error: Files _build/default/needs-fixes.mld and
  _build/default/.mdx/needs-fixes.mld.corrected differ.
  [1]
  $ dune promote
  Promoting _build/default/.mdx/needs-fixes.mld.corrected to needs-fixes.mld.
  $ dune runtest

We also make sure that `:standard` resolves properly:

  $ cat > dune <<EOF
  > (mdx
  >   (files :standard))
  > EOF
  $ cat > needs-fixes.mld <<EOF
  > This is a sample mld file. It has some code that is invalid.
  >  
  > {[
  >   # List.map (fun x -> x * x) [(1 + 9); 2; 3; 4];;
  >   - : int list = [1; 2; 3; 8]
  > ]}
  > 
  > A run of MDX should output a fixed version.
  > EOF
  $ dune runtest
  File "needs-fixes.mld", line 1, characters 0-0:
  Error: Files _build/default/needs-fixes.mld and
  _build/default/.mdx/needs-fixes.mld.corrected differ.
  [1]
  $ dune promote
  Promoting _build/default/.mdx/needs-fixes.mld.corrected to needs-fixes.mld.
  $ dune runtest
