Demonstrate that local dependencies that are marked as {with-test} can be
included.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg post <<EOF
  > EOF
  $ mkpkg build <<EOF
  > EOF
  $ mkpkg dev <<EOF
  > EOF
  $ mkpkg test <<EOF
  > EOF
  $ mkpkg doc <<EOF
  > EOF
  $ mkpkg dev-setup <<EOF
  > EOF

Note that dune solves packages with with-doc always set to false, so
documentation-only deps are omitted from the solution.
  $ solve "(test :with-test) (doc :with-doc) (dev-setup :with-dev-setup) (dev :with-dev) (build :build) (post :post)"
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Unexpected exception raised while solving dependencies",
    { exception = "Invalid_argument(\"filter_deps\")" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
