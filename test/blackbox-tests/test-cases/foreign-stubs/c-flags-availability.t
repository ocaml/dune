Availability errors belong in the standard C and C++ flags when supported.
The compiler wrapper controls __has_warning so this also runs without Clang.
We inspect rules rather than pass the simulated capability's flag to GCC.

  $ actual_ocamlc=$(command -v ocamlc)
  $ set -- $(ocamlc -config-var c_compiler)
  $ actual_cc=$(command -v "$1")
  $ shift
  $ actual_cc_args="$*"
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
  > exec "$actual_cc" $actual_cc_args \\
  >   -Wno-builtin-macro-redefined -U__has_warning \\
  >   '-D__has_warning(x)=1' "\$@"
  > EOF
  $ chmod +x ocamlc-wrapper cc-wrapper
  $ mkdir findlib
  $ cat >findlib/findlib.conf <<EOF
  > path="$external_findlib_path"
  > ocamlc="$PWD/ocamlc-wrapper"
  > EOF
  $ export OCAMLFIND_CONF=$PWD/findlib/findlib.conf
  $ unset OCAMLFIND_TOOLCHAIN
  $ make_dune_project 3.0
  $ touch stub.c stubxx.cpp
  $ cat >dune <<'EOF'
  > (library
  >  (name test)
  >  (modules)
  >  (foreign_stubs (language c) (names stub))
  >  (foreign_stubs (language cxx) (names stubxx)))
  > EOF
  $ show_flags () {
  >   dune rules --format=json "$@" stub.o stubxx.o \
  >     | jq -c 'include "dune"; .[]
  >       | {target: (ruleTargets[0] | basename),
  >          availability: ([ruleActionRunArgv[]
  >            | select(. == "-Werror=unguarded-availability-new")]
  >            | length)}' \
  >     | sort
  > }

The compiler supports the warning, so Dune makes it fatal.

  $ show_flags
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}

Both languages share a single compiler probe, reused on the next invocation.

  $ dune trace cat | jq -s '[.[]
  >   | select(.cat == "process" and .name == "finish" and
  >       (.args.prog | endswith("/cc-wrapper")))] | length'
  1
  $ show_flags
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}
  $ dune trace cat | jq -s '[.[]
  >   | select(.cat == "process" and .name == "finish" and
  >       (.args.prog | endswith("/cc-wrapper")))] | length'
  0

Release builds and older language versions need the same protection.

  $ show_flags --profile release
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}
  $ make_dune_project 2.8
  $ echo '(use_standard_c_and_cxx_flags true)' >>dune-project
  $ show_flags
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}
  $ make_dune_project 3.0

Do not duplicate the flag when OCaml already supplies it.

  $ sed -i.bak \
  >   's/ocamlc_cflags:|/ocamlc_cflags: -Werror=unguarded-availability-new|/' \
  >   ocamlc-wrapper
  $ show_flags
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}

Only C inherits ocamlc_cppflags, so C++ still needs its own default.

  $ sed -i.bak \
  >   -e 's/ocamlc_cflags: -Werror=unguarded-availability-new|/ocamlc_cflags:|/' \
  >   -e 's/ocamlc_cppflags:|/ocamlc_cppflags: -Werror=unguarded-availability-new|/' \
  >   ocamlc-wrapper
  $ show_flags
  {"target":"stub.o","availability":1}
  {"target":"stubxx.o","availability":1}
  $ sed -i.bak \
  >   's/ocamlc_cppflags: -Werror=unguarded-availability-new|/ocamlc_cppflags:|/' \
  >   ocamlc-wrapper

Users can remove the default through the ordered-set language.

  $ cat >>dune <<'EOF'
  > (env
  >  (_
  >   (c_flags (:standard \ -Werror=unguarded-availability-new))
  >   (cxx_flags (:standard \ -Werror=unguarded-availability-new))))
  > EOF
  $ show_flags
  {"target":"stub.o","availability":0}
  {"target":"stubxx.o","availability":0}
  $ sed -i.bak '/^(env/,$d' dune

Unsupported warnings must not be added, including when __has_warning itself
is unavailable. Changing the compiler wrapper invalidates the cached probe.

  $ sed -i.bak 's/__has_warning(x)=1/__has_warning(x)=0/' cc-wrapper
  $ show_flags
  {"target":"stub.o","availability":0}
  {"target":"stubxx.o","availability":0}
  $ sed -i.bak "s/'-D__has_warning(x)=0' //" cc-wrapper
  $ show_flags
  {"target":"stub.o","availability":0}
  {"target":"stubxx.o","availability":0}
