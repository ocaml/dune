Test that extra files associated with a package are handled correctly when
multiple different versions of the package are present in the lockdir.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ VERSION1_FILE=mock-opam-repository/packages/foo/foo.1/files/version.txt
  $ VERSION2_FILE=mock-opam-repository/packages/foo/foo.2/files/version.txt

  $ mkdir -p $(dirname $VERSION1_FILE)
  $ echo version_1 > $VERSION1_FILE

  $ mkdir -p $(dirname $VERSION2_FILE)
  $ echo version_2 > $VERSION2_FILE

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION1_FILE | cut -f1 -d' ')"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION2_FILE | cut -f1 -d' ')"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ mkpkg bar <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > depends: [
  >   "foo" {= "1" & os = "linux"}
  >   "foo" {= "2" & os = "macos"}
  > ]
  > EOF

Define a project with a package depending on bar:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends bar))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Solve the project. The SAT-level cross-platform version equality constraint
rejects bar's per-platform version disagreement on foo, so the lock fails:
  $ dune pkg lock
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  The dependency solver failed to find a solution for the following platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 x.dev
  - foo -> foo.1
      bar 0.0.1 requires = 1
  [1]


No solution exists so the lockdir has no foo.<ver>.files directories:
  $ cat ${default_lock_dir}/foo.1.files/version.txt
  cat: dune.lock/foo.1.files/version.txt: No such file or directory
  [1]
  $ cat ${default_lock_dir}/foo.2.files/version.txt
  cat: dune.lock/foo.2.files/version.txt: No such file or directory
  [1]

Without a lockdir, building errors out regardless of platform.

Build as if we're on linux:
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat ${default_lock_dir}/foo.1.files/version.txt
  cat: dune.lock/foo.1.files/version.txt: No such file or directory
  [1]

  $ dune clean

Build as if we're on macos:
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat ${default_lock_dir}/foo.2.files/version.txt
  cat: dune.lock/foo.2.files/version.txt: No such file or directory
  [1]
