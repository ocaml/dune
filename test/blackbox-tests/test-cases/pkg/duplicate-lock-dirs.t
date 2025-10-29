We configure the same lock directory twice:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir (path foo))
  > (lock_dir (path foo))
  > EOF

  $ dune build
  File "dune-workspace", line 3, characters 0-21:
  3 | (lock_dir (path foo))
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Lock directory "foo" is defined multiple times:
  - dune-workspace:2
  - dune-workspace:3
  [1]

Another way to define a duplicate is by omitting the path. Since it defaults to
.dune-solution-cache:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir)
  > (lock_dir (path .dune-solution-cache))
  > EOF

  $ dune build
  File "dune-workspace", line 3, characters 0-27:
  3 | (lock_dir (path .dune-solution-cache))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Lock directory ".dune-solution-cache" is defined multiple times:
  - dune-workspace:2
  - dune-workspace:3
  [1]

However, defining a single lock_dir at the default path is allowed, since it
explicitly configures the default:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir)
  > EOF

  $ dune build
