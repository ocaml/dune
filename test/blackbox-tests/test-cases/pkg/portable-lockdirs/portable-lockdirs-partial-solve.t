Demonstrate the case where a project can only be solved for a subset of platforms.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package that is only available on macos.
  $ mkpkg foo <<EOF
  > available: os = "macos"
  > build: [
  >   ["mkdir" "-p" "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Solving will still succeed, but there'll be a warning because dune will attempt
to solve for macos, linux, and windows by default.
  $ dune pkg lock --trace-file trace.json
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1
  
  No package solution was found for some requsted platforms.
  
  Platforms with no solution:
  - arch = arm64; os = linux
  - arch = x86_64; os = linux
  
  See the trace file with --trace-file for more details. Configure platforms to
  solve for in the dune-workspace file.

The log file will contain errors about the package being unavailable.
  $ jqScript=$(mktemp)
  $ cat >$jqScript <<EOF
  > .[] |
  > select(.cat == "log" and .args.message != "ocamlparam" and (.args.message | contains("Shared cache") | not)) |
  > .args
  > EOF
  $ jq -f $jqScript trace.json
  {
    "message": "Workspace root",
    "root": "$TESTCASE_ROOT"
  }
  {
    "message": "Solver found partial solution",
    "error_count": "1"
  }
  {
    "message": "Dependency solution",
    "lock_dir": "dune.lock",
    "packages": [
      "foo.0.0.1"
    ]
  }

The lockdir will contain a list of the platforms where solving succeeded.
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (dependency_hash 36e640fbcda71963e7e2f689f6c96c3e)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

No errors when you try to build the platform on macos.
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build

Building on linux fails because the lockdir doesn't contain a compatible solution.
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  File "dune.lock/lock.dune", lines 10-13, characters 1-58:
  10 |  ((arch x86_64)
  11 |   (os macos))
  12 |  ((arch arm64)
  13 |   (os macos)))
  Error: The lockdir does not contain a solution compatible with the current
  platform.
  The current platform is:
  - arch = arm64
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  - sys-ocaml-version = 5.4.0+fake
  Hint: Try adding the following to dune-workspace:
  Hint: (lock_dir (solve_for_platforms ((arch arm64) (os linux))))
  Hint: ...and then rerun 'dune pkg lock'
  [1]
