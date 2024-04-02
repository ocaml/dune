Test that dune can dynamically select a lockdir with a cond statement 

  $ . ./helpers.sh

  $ mkrepo

  $ mkpkg arm64-only <<EOF
  > install: [ "echo" "arm64-only" ]
  > EOF

  $ mkpkg linux-only <<EOF
  > install: [ "echo" "linux-only" ]
  > EOF

  $ mkpkg macos-only <<EOF
  > install: [ "echo" "macos-only" ]
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (path dune.macos.arm64.lock)
  >  (repositories mock)
  >  (solver_env
  >   (arch arm64)
  >   (os macos)))
  > (lock_dir
  >  (path dune.macos.lock)
  >  (repositories mock)
  >  (solver_env
  >   (arch amd64)
  >   (os macos)))
  > (lock_dir
  >  (path dune.linux.lock)
  >  (repositories mock)
  >  (solver_env
  >   (arch amd64)
  >   (os linux)))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > (context
  >  (default
  >   (lock_dir (cond
  >    ((and (= %{architecture} arm64) (= %{system} macosx)) dune.macos.arm64.lock)
  >    ((= %{system} macosx) dune.macos.lock)
  >    ((= %{system} linux) dune.linux.lock)))))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (depends
  >   (arm64-only (= :arch arm64))
  >   (macos-only (= :os macos))
  >   (linux-only (= :os linux))))
  > EOF

Generate all lockdirs:
  $ dune pkg lock dune.macos.arm64.lock
  Solution for dune.macos.arm64.lock:
  - arm64-only.0.0.1
  - macos-only.0.0.1
  $ dune pkg lock dune.macos.lock
  Solution for dune.macos.lock:
  - macos-only.0.0.1
  $ dune pkg lock dune.linux.lock
  Solution for dune.linux.lock:
  - linux-only.0.0.1

Demonstrate that the correct lockdir is being chosen by building packages that
are only dependent on on certain systems.

Build macos package on macos:
  $ dune clean
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=arm64 dune build _build/_private/default/.pkg/macos-only/target/
  macos-only

Build macos package on macos:
  $ dune clean
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=amd64 dune build _build/_private/default/.pkg/macos-only/target/
  macos-only

Build linux package on macos (will fail):
  $ dune clean
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=amd64 dune build _build/_private/default/.pkg/linux-only/target/
  Error: Unknown package "linux-only"
  [1]

Build macos package on linux (will fail):
  $ dune clean
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=amd64 dune build _build/_private/default/.pkg/macos-only/target/
  Error: Unknown package "macos-only"
  [1]

Build linux package on linux:
  $ dune clean
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=amd64 dune build _build/_private/default/.pkg/linux-only/target/
  linux-only

Try setting the os to one which doesn't have a corresponding lockdir:
  $ dune clean
  $ DUNE_CONFIG__OS=windows dune build _build/_private/default/.pkg/linux-only/target/
  File "dune-workspace", lines 28-30, characters 3-162:
  28 |    ((and (= %{architecture} arm64) (= %{system} macosx)) dune.macos.arm64.lock)
  29 |    ((= %{system} macosx) dune.macos.lock)
  30 |    ((= %{system} linux) dune.linux.lock)))))
  Error: None of the conditions matched so no lockdir could be chosen.
  [1]

Test that cond statements can have a default value:
  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (solver_env
  >   (arch amd64)
  >   (os linux))
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > (context
  >  (default
  >   (lock_dir (cond
  >    (false non-existant)
  >    (default dune.lock)))))
  > EOF

  $ dune pkg lock dune.lock
  Solution for dune.lock:
  - linux-only.0.0.1
  $ dune clean
  $ dune build _build/_private/default/.pkg/linux-only/target/
  linux-only
