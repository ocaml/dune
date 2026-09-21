Reject calls to APIs introduced after the deployment target (issue #16413).
Clang can compile a header-free Darwin object on other hosts, so this test
needs neither a macOS SDK nor a particular version of its system headers.

  $ actual_ocamlc=$(command -v ocamlc)
  $ clang=$(command -v clang)
  $ external_findlib_path=$(ocamlfind printconf path | tr '\n' ':' | sed 's/:$//')
  $ cat >ocamlc-wrapper <<EOF
  > #!/bin/sh
  > if test "\$1" = -config; then
  >   "$actual_ocamlc" -config \\
  >     | sed -e 's|^c_compiler:.*|c_compiler: $PWD/cc-wrapper|' \\
  >           -e 's|^ocamlc_cflags:.*|ocamlc_cflags:|' \\
  >           -e 's|^ocamlc_cppflags:.*|ocamlc_cppflags:|'
  > else
  >   exec "$actual_ocamlc" "\$@"
  > fi
  > EOF
  $ cat >cc-wrapper <<EOF
  > #!/bin/sh
  > exec "$clang" --target=x86_64-apple-macos11 "\$@"
  > EOF
  $ chmod +x ocamlc-wrapper cc-wrapper

Older or target-restricted Clang installations may lack the required features.
Only run the compiler-dependent assertions when this independent probe succeeds.
The portable companion test still checks flag selection on other compilers.

  $ cat >probe.c <<'EOF'
  > #if !__has_warning("-Wunguarded-availability-new")
  > #error unsupported availability warning
  > #endif
  > void probe(void) {
  >   if (__builtin_available(macos 99.0, *)) {}
  > }
  > EOF
  $ supported=false
  $ if ./cc-wrapper -Werror=unguarded-availability-new -c probe.c \
  >      -o probe.o >probe.log 2>&1; then
  >   supported=true
  > fi

  $ mkdir findlib
  $ cat >findlib/findlib.conf <<EOF
  > path="$external_findlib_path"
  > ocamlc="$PWD/ocamlc-wrapper"
  > EOF
  $ export OCAMLFIND_CONF=$PWD/findlib/findlib.conf
  $ unset OCAMLFIND_TOOLCHAIN
  $ make_dune_project 3.0
  $ cat >stub.c <<'EOF'
  > extern void future_api(void)
  >   __attribute__((availability(macos, introduced=99.0)));
  > void stub(void) { future_api(); }
  > EOF
  $ cp stub.c stubxx.cpp
  $ cat >dune <<'EOF'
  > (library
  >  (name test)
  >  (modules)
  >  (foreign_stubs (language c) (names stub))
  >  (foreign_stubs (language cxx) (names stubxx)))
  > EOF

Both builds reject the unsafe call. Check only the availability diagnostic,
not compiler-dependent source excerpts and notes.

  $ if $supported; then
  >   dune build stub.o >c.log 2>&1; result=$?
  >   test "$result" = 1 &&
  >     grep -q "error: 'future_api' is only available on macOS" c.log
  > fi
  $ if $supported; then
  >   dune build stubxx.o --profile release >cxx.log 2>&1; result=$?
  >   test "$result" = 1 &&
  >     grep -q "error: 'future_api' is only available on macOS" cxx.log
  > fi

Removing the flag restores a warning rather than an error.

  $ cat >>dune <<'EOF'
  > (env
  >  (_
  >   (c_flags (:standard \ -Werror=unguarded-availability-new))
  >   (cxx_flags (:standard \ -Werror=unguarded-availability-new))))
  > EOF
  $ if $supported; then
  >   dune build stub.o >c.log 2>&1; result=$?
  >   test "$result" = 0 &&
  >     grep -q "warning: 'future_api' is only available on macOS" c.log
  > fi
  $ if $supported; then
  >   dune build stubxx.o --profile release >cxx.log 2>&1; result=$?
  >   test "$result" = 0 &&
  >     grep -q "warning: 'future_api' is only available on macOS" cxx.log
  > fi

An availability guard makes both calls safe with the default flags.

  $ sed -i.bak '/^(env/,$d' dune
  $ cat >stub.c <<'EOF'
  > extern void future_api(void)
  >   __attribute__((availability(macos, introduced=99.0)));
  > void stub(void) {
  >   if (__builtin_available(macos 99.0, *)) future_api();
  > }
  > EOF
  $ cp stub.c stubxx.cpp
  $ if $supported; then dune build stub.o stubxx.o; fi
