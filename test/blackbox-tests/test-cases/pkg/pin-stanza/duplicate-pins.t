Test cases where pins are duplicated

  $ mkrepo
  $ add_mock_repo_if_needed

Create a dependency that we can depend on.

  $ mkdir _dep_a
  $ cat > _dep_a/dune-project <<EOF
  > (lang dune 3.22)
  > (generate_opam_files true)
  > (package
  >  (name dep_a))
  > EOF
  $ cat > _dep_a/dune <<EOF
  > (library
  >  (public_name dep_a))
  > EOF
  $ cat > _dep_a/dep_a.ml <<EOF
  > let dep = "A"
  > EOF

Also generate an .opam file to be able to pin from OPAM.

  $ (cd _dep_a && dune build dep_a.opam)

In our main project we depend on the same pin twice.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (pin
  >  (url file://$PWD/_dep_a)
  >  (package (name dep_a)))
  > (pin
  >  (url file://$PWD/_dep_a)
  >  (package (name dep_a)))
  > EOF

We expect that locking will fail as we're trying to pin the same package
multiple times:

  $ dune pkg lock
  File "dune-project", line 4, characters 1-23:
  4 |  (package (name dep_a)))
       ^^^^^^^^^^^^^^^^^^^^^^
  Error: package "dep_a" is already defined
  [1]

We define another package, `dep_b` which depends on `dep_a`

  $ mkdir _dep_b
  $ cat > _dep_b/dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat > _dep_b/dune <<EOF
  > (library
  >  (public_name dep_b)
  >  (libraries dep_a))
  > EOF
  $ cat > _dep_b/dep_b.ml <<EOF
  > let dep = Printf.sprintf "%sB" Dep_a.dep
  > EOF

We create a fork of `dep_a` in the `dep_c` folder and modify it to display
something different:

  $ cp -r _dep_a _dep_c
  $ cat > _dep_c/dep_a.ml <<EOF
  > let dep = "C"
  > EOF

`dep_b` pins `dep_a` via the `pin-depends` but it pins the fork that displays
"C":

  $ cat > _dep_b/dep_b.opam << EOF
  > opam-version: "2.0"
  > pin-depends: [ "dep_a" "file://$PWD/_dep_c" ]
  > depends: ["dep_a"]
  > build: ["dune" "build" "-p" name "@install"]
  > EOF

In our main project we pin `dep_a` to the original version via the pin stanza,
as well as `dep_b`.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (pin
  >  (url file://$PWD/_dep_a)
  >  (package (name dep_a)))
  > (pin
  >  (url file://$PWD/_dep_b)
  >  (package (name dep_b)))
  > (package
  >  (name main)
  >  (depends dep_b))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dep_b))
  > EOF

  $ cat > main.ml <<EOF
  > print_endline Dep_b.dep
  > EOF

This is accepted by the solver, despite `dep_a` being pinned to different
locations via Dune and OPAM:

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - dep_a.dev
  - dep_b.dev

Executing the binary shows that the pinning from Dune has taken preference over
the `pin-depends` stanza in `dep_b`:

  $ dune exec ./main.exe
  AB
