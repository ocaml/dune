Reproduction case for issue #5485: rules targeting the Windows-reserved device
name NUL produce a confusing error during sandbox promotion and may leave NUL
in the build directory, causing `dune clean` to fail.

On Windows, NUL is a reserved device name (equivalent to Unix /dev/null).
A `(with-stdout-to NUL ...)` rule using the literal string NUL -- rather than
the platform-neutral `%{null}` variable -- treats NUL as a regular file target.
This pattern can appear in practice when a library uses Windows-style output
suppression, e.g. to discard tool output during a configure step.

The original report showed a NUL file left in _build/default/src/unix/ after
a failed build, preventing `dune clean` from removing the directory:

  Error: rmdir(_build\default\src\unix): Directory not empty

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (deps
  >   (sandbox always))
  >  (action
  >   (with-stdout-to NUL (run echo hello))))
  > EOF

  $ dune build 2>&1 | sed 's|\.sandbox/[0-9a-f]*/|.sandbox/SANDBOXHASH/|g'
  Error:
  rename(_build/.sandbox/SANDBOXHASH/default/NUL): File exists
  -> required by _build/default/NUL
  -> required by alias all
  -> required by alias default

  $ dune clean
