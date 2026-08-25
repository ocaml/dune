An installed library can advertise the intermediate foreign object used to
build its stub archive without installing that object. A package dependency
must retain the usable library and its stubs, not require the absent object.

  $ mkdir -p source/b prefix dune-consumer meta-consumer
  $ cat >source/dune-project <<'EOF'
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > EOF
  $ cat >source/dune <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let value = B.value + 1' >source/a.ml
  $ cat >source/b/dune <<'EOF'
  > (library
  >  (public_name b)
  >  (modes byte)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ cat >source/b/b.ml <<'EOF'
  > external stub : unit -> int = "b_stubs_value"
  > let value = stub ()
  > EOF
  $ cat >source/b/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_stubs_value(value unit) { return Val_int(1); }
  > EOF
  $ dune build --root source @install
  $ dune install --root source --prefix "$PWD/prefix" 2>/dev/null

Verify the advertised-but-uninstalled object and the actual installed stubs.
Use the compiler's platform-specific suffixes throughout.

  $ ext_obj=$(ocamlc -config-var ext_obj)
  $ ext_lib=$(ocamlc -config-var ext_lib)
  $ ext_dll=$(ocamlc -config-var ext_dll)
  $ b_lib="$PWD/prefix/lib/b"
  $ stub_archive="$b_lib/libb_stubs$ext_lib"
  $ stub_dll="$b_lib/../stublibs/dllb_stubs$ext_dll"
  $ foreign_object="$b_lib/b_stubs$ext_obj"
  $ grep -Fq "foreign_objects b_stubs$ext_obj" "$b_lib/dune-package"
  $ test ! -e "$foreign_object"
  $ test -f "$stub_archive"
  $ test -f "$stub_dll"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ export CAML_LD_LIBRARY_PATH="$PWD/prefix/lib/stublibs"

The consumer actually calls the C primitive. It must build and run even
though b's intermediate object is absent.

  $ echo '(lang dune 3.24)' >dune-consumer/dune-project
  $ echo 'let () = Printf.printf "%d\n" A.value' >dune-consumer/main.ml
  $ cat >dune-consumer/dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps main.ml (package a))
  >  (action
  >   (run ocamlfind ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF
  $ cp dune-consumer/dune dune-consumer/dune-project \
  > dune-consumer/main.ml meta-consumer/
  $ dune build --root dune-consumer main.exe
  $ dune-consumer/_build/default/main.exe
  2

CR-someday alizter: The required library and installed stubs should be tracked.
The uninstalled object must remain absent from the action's dependencies.

  $ dune rules --root dune-consumer --format=json main.exe >dune-rules.json
  $ jq_dune --arg b "$b_lib" --arg archive "$stub_archive" \
  > --arg dll "$stub_dll" --arg object "$foreign_object" '
  >   rulesMatchingTarget("main.exe") | {
  >     reader: "dune-package",
  >     required_interface:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cmi"; $b; "*.cmi"),
  >     required_archive:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cma"; $b; "*.cma"),
  >     required_metadata: ruleHasDepFile($b + "/dune-package"),
  >     stub_archive: ruleHasDepFile($archive),
  >     stub_dll: ruleHasDepFile($dll),
  >     uninstalled_object: ruleHasDepFile($object)
  >   }' dune-rules.json
  {
    "reader": "dune-package",
    "required_interface": false,
    "required_archive": false,
    "required_metadata": false,
    "stub_archive": false,
    "stub_dll": false,
    "uninstalled_object": false
  }

Keep the same compile/run coverage through META. The foreign object field is
specific to dune-package, but the usable library and stub archive must still
be retained by the closure.

  $ rm prefix/lib/a/dune-package "$b_lib/dune-package"
  $ test -f "$b_lib/META"
  $ dune build --root meta-consumer main.exe
  $ meta-consumer/_build/default/main.exe
  2
  $ dune rules --root meta-consumer --format=json main.exe >meta-rules.json
  $ jq_dune --arg b "$b_lib" --arg archive "$stub_archive" '
  >   rulesMatchingTarget("main.exe") | {
  >     reader: "META",
  >     required_interface:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cmi"; $b; "*.cmi"),
  >     required_archive:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cma"; $b; "*.cma"),
  >     required_metadata: ruleHasDepFile($b + "/META"),
  >     stub_archive: ruleHasDepFile($archive)
  >   }' meta-rules.json
  {
    "reader": "META",
    "required_interface": false,
    "required_archive": false,
    "required_metadata": false,
    "stub_archive": false
  }
