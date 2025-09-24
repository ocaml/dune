Exercise the workflow when package management is explicitly disabled.

When (pkg disabled) is set, dune should behave as if no lock directory exists.
This tests the three-state logic: None (auto-detect), Some true (force enable),
Some false (force disable).

This means that:
1. Package rules should not be loaded during the build.
2. All pkg commands should fail with appropriate errors.
3. This should work even when lock directories are present.
4. Explicit settings should override auto-detection.

  $ . ./helpers.sh
  $ mkrepo

First, create a dummy library package in a subdirectory that we can depend on:

  $ mkdir -p external_sources/testlib_src
  $ cat > external_sources/testlib_src/dune-project << EOF
  > (lang dune 3.20)
  > (package (name testlib))
  > EOF

  $ cat > external_sources/testlib_src/dune << EOF
  > (library
  >  (public_name testlib)
  >  (name testlib))
  > EOF

  $ cat > external_sources/testlib_src/testlib.ml << EOF
  > let greet name = Printf.printf "Hello %s from testlib!\n" name
  > EOF

Create a mock opam package for our library:

  $ mkpkg testlib << EOF
  > build: [
  >   ["dune" "build" "-p" name "@install"]
  > ]
  > EOF

Setup a project with dependencies that would normally require package
management:

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends testlib))
  > EOF

  $ cat > dune << EOF
  > (dirs (:standard \ external_sources))
  > (executable
  >  (public_name foo)
  >  (name foo)
  >  (libraries testlib))
  > EOF

  $ cat > foo.ml << EOF
  > let () = Testlib.greet "world"
  > EOF

Create a workspace configuration that explicitly disables package management:

  $ add_mock_repo_if_needed
  $ disable_pkg

Test that dune pkg enabled reports disabled:

  $ dune pkg enabled
  [1]

Test that pkg commands fail when disabled:

  $ dune pkg lock
  File "dune-workspace", line 2, characters 5-13:
  2 | (pkg disabled)
           ^^^^^^^^
  Error: Package management is disabled in workspace configuration.
  Hint: To enable package management, remove the explicit (pkg disabled)
  setting from your dune-workspace file.
  [1]

  $ dune pkg outdated 2>&1 | grep "Error:"
  Error: Package management is disabled in workspace configuration.

  $ dune pkg validate-lockdir 2>&1 | grep "Error:"
  Error: Package management is disabled in workspace configuration.

Test that dune build fails appropriately when trying to use unavailable
packages, proving we did not install the needed dependency:

  $ dune build 2>&1 | grep -E "(Error|Library.*not found)" | head -5
  Error: Library "testlib" not found.

Now create a lock directory with our testlib package to simulate the case
where lockdirs exist but pkg is disabled. First, temporarily enable pkg to
create the lock file:

  $ create_mock_repo
  $ enable_pkg

  $ dune pkg lock
  Solution for dune.lock:
  - testlib.0.0.1

Now disable pkg again:

  $ create_mock_repo  
  $ disable_pkg

Even with a proper lock directory present, pkg should still be reported as
disabled:

  $ dune pkg enabled
  [1]

And dune build should still fail to find the library because pkg rules are not
loaded:

  $ dune build 2>&1 | grep -E "(Error|Library.*not found)" | head -5
  Error: Library "testlib" not found.

This behavior is identical to having no lock directory. When pkg is disabled,
the build system does not load package rules, so it does not know about the
packages.

Test enabling package management again - it should work normally:

  $ create_mock_repo
  $ enable_pkg

  $ dune pkg enabled

With pkg enabled and a lock directory present, package management is now
active.

Now test the default (auto-detect) behavior:

  $ create_mock_repo
  $ unset_pkg

With lockdir present, auto-detect should enable pkg management:

  $ dune pkg enabled

Remove lockdir, auto-detect should disable pkg management:

  $ rm -rf ${default_lock_dir}
  $ dune pkg enabled
  [1]

Test that auto-detect can be overridden by explicit workspace settings:

  $ mkdir -p ${default_lock_dir}
  $ cat > ${default_lock_dir}/lock.dune << EOF
  > (lang package-lock 0.1)
  > EOF

  $ create_mock_repo
  $ disable_pkg

Even with lockdir present, explicit disabled should take precedence:

  $ dune pkg enabled
  [1]

And explicit enabled should work even without lockdir:

  $ rm -rf ${default_lock_dir}
  $ create_mock_repo
  $ enable_pkg

  $ dune pkg enabled
