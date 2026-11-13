A generated header included by a ctypes preamble must be a dependency of the
C type probe, without an explicit ctypes deps field. Reproduces #16834.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.7)
  > (using ctypes 0.3)
  > EOF

  $ cat > dune <<'EOF'
  > (library
  >  (name l)
  >  (ctypes
  >   (external_library_name none)
  >   (build_flags_resolver (vendored (c_flags -I .)))
  >   (headers (preamble "#include <local.h>"))
  >   (type_description (functor bindings) (instance types))
  >   (generated_entry_point c)))
  > (rule
  >  (target local.h)
  >  (action (with-stdout-to %{target} (echo "#define ANSWER 42"))))
  > EOF

  $ cat > bindings.ml <<'EOF'
  > module Types (F : Ctypes.TYPE) = struct
  >   let answer = F.constant "ANSWER" F.int
  > end
  > EOF

Keep the missing-header diagnostic, normalizing GCC/Clang formatting.

  $ dune build 2>&1 | sed -n \
  >   -e "s/'local.h' file not found/local.h: No such file or directory/" \
  >   -e 's/.*fatal error: /fatal error: /p'
  fatal error: local.h: No such file or directory
  [1]
