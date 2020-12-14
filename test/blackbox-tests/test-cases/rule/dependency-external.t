rules with dependencies outside the build dir are allowed

  $ mkdir -p a/b

  $ cat >a/b/dune-project <<EOF
  > (lang dune 2.8)
  > EOF

### absolute path

## Test absolute
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "$(pwd)/external.txt" (run cat -))))
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

## Test copy files absolute
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files "$(pwd)/external.txt")
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

### 1 level below

## Test relative 1 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../external.txt" (run cat -))))
  > EOF

  $ cat >a/external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Fs_memo.Watcher.watch called on a build path",
    { path = In_build_dir "external.txt" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Dune_engine__Fs_memo.Watcher.watch in file
    "src/dune_engine/fs_memo.ml", line 129, characters 6-108
  Called from Dune_engine__Fs_memo.file_digest in file
    "src/dune_engine/fs_memo.ml", line 271, characters 12-60
  Called from Dune_engine__Load_rules.source_file_digest in file
    "src/dune_engine/load_rules.ml", line 282, characters 2-26
  Called from Dune_engine__Load_rules.get_rule_or_source in file
    "src/dune_engine/load_rules.ml", line 871, characters 13-36
  Called from Dune_engine__Build_system.Exported.build_file_impl in file
    "src/dune_engine/build_system.ml", line 888, characters 4-38
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("build-file", In_build_dir "external.txt")
  -> required by ("<unnamed>", ())
  -> required by ("build-alias", { dir = "default"; name = "test" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Fs_memo.Watcher.watch called on a build path",
    { path = In_build_dir "external.txt" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Dune_engine__Fs_memo.Watcher.watch in file
    "src/dune_engine/fs_memo.ml", line 129, characters 6-108
  Called from Dune_engine__Fs_memo.file_digest in file
    "src/dune_engine/fs_memo.ml", line 271, characters 12-60
  Called from Dune_engine__Load_rules.source_file_digest in file
    "src/dune_engine/load_rules.ml", line 282, characters 2-26
  Called from Dune_engine__Load_rules.get_rule_or_source in file
    "src/dune_engine/load_rules.ml", line 871, characters 13-36
  Called from Dune_engine__Build_system.Exported.build_file_impl in file
    "src/dune_engine/build_system.ml", line 888, characters 4-38
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("build-file", In_build_dir "external.txt")
  -> required by ("<unnamed>", ())
  -> required by ("build-alias", { dir = "default"; name = "test" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

  $ cat >a/external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Fs_memo.Watcher.watch called on a build path",
    { path = In_build_dir "external.txt" })
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml",
    line 11, characters 30-62
  Called from Dune_engine__Fs_memo.Watcher.watch in file
    "src/dune_engine/fs_memo.ml", line 129, characters 6-108
  Called from Dune_engine__Fs_memo.file_digest in file
    "src/dune_engine/fs_memo.ml", line 271, characters 12-60
  Called from Dune_engine__Load_rules.source_file_digest in file
    "src/dune_engine/load_rules.ml", line 282, characters 2-26
  Called from Dune_engine__Load_rules.get_rule_or_source in file
    "src/dune_engine/load_rules.ml", line 871, characters 13-36
  Called from Dune_engine__Build_system.Exported.build_file_impl in file
    "src/dune_engine/build_system.ml", line 888, characters 4-38
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/scheduler.ml", line 69,
    characters 8-11
  -> required by ("build-file", In_build_dir "external.txt")
  -> required by ("<unnamed>", ())
  -> required by ("build-alias", { dir = "default"; name = "test" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

## Test copy files 1 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files ../external.txt)
  > EOF

  $ cat >a/external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

  $ cat >a/external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

### 2 level below

## Test relative 2 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../../external.txt" (run cat -))))
  > EOF

  $ rm -f a/external.txt

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

## Test copy files 2 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files ../../external.txt)
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-30:
  4 | (copy_files ../../external.txt)
                  ^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from .
  [1]

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-30:
  4 | (copy_files ../../external.txt)
                  ^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from .
  [1]

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  File "dune", line 4, characters 12-30:
  4 | (copy_files ../../external.txt)
                  ^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from .
  [1]

## Test dune exec absolute
  $ cat >a/script.sh <<EOF
  > #!/bin/sh
  >  echo "txt1"
  > EOF

  $ chmod u+x a/script.sh

  $ dune exec --root=a/b -- $(pwd)/a/script.sh
  Entering directory 'a/b'
  txt1

## Test dune exec 1 level below
  $ dune exec --root=a/b -- ../script.sh
  Entering directory 'a/b'
  File "dune", line 4, characters 12-30:
  4 | (copy_files ../../external.txt)
                  ^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from .
  [1]

## Test dune exec 2 level below
  $ mv a/script.sh .

  $ dune exec --root=a/b -- ../../script.sh
  Entering directory 'a/b'
  Error: path outside the workspace: ../../script.sh from default
  [1]
