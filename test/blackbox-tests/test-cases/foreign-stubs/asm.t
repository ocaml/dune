Here we test the assembly language rules for stubs and foreign libraries.

  $ cat > dune-project <<EOF
  > (lang dune 3.26)
  > EOF

We build an archive from an assembly file, without mixed-language declarations
or language inference.

  $ cat > dune <<EOF
  > (foreign_library
  >  (names hello)
  >  (language asm)
  >  (archive_name mylib))
  > EOF

We add a rule for building the assembly file. This will not interfere with our archive.
  $ cat >> dune <<EOF
  > (rule
  >  (enabled_if
  >   (<> %{system} "win32"))
  >  (target hello.s)
  >  (deps
  >   (sandbox always)
  >   hello.c)
  >  (action
  >   (progn
  >    (run %{ocaml-config:c_compiler} -S hello.c))))
  > 
  > (rule
  >  (enabled_if
  >   (= %{system} "win32"))
  >  (target hello.asm)
  >  (deps
  >   (sandbox always)
  >   hello.c)
  >  (action
  >   (progn
  >    (run %{ocaml-config:c_compiler} /FA hello.c))))
  > EOF

Here is the C file we will turn into assembly.
  $ cat > hello.c <<EOF
  > char *message() { return "Hello, world!\n"; };
  > EOF

We can build the archive.
  $ dune build libmylib.a

We test that the contents of the archive contain our message.
  $ nm _build/default/libmylib.a | grep message > /dev/null

Assembly sources are not mistaken for C sources in the compilation database.

  $ dune build @check
  $ jq -c 'map(.file)' compile_commands.json
  []

Assembly flags support ordered-set includes and variable expansion.
C/C++ flags do not leak into assembly compilation.

  $ mkdir flags
  $ cd flags
  $ make_dune_project 3.26
  $ cat > flags.S <<'EOF'
  > #ifndef ASM_FLAG
  > #error assembly flags were ignored
  > #elif ASM_VALUE != 42
  > #error incorrect assembly flag value
  > #endif
  > #if defined(C_ONLY) || defined(CXX_ONLY)
  > #error C/C++ flags leaked into assembly compilation
  > #endif
  > EOF
  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (c_flags -DC_ONLY)
  >   (cxx_flags -DCXX_ONLY)))
  > (rule
  >  (target asm_flags.sexp)
  >  (action (write-file %{target} "(-DASM_FLAG)")))
  > (foreign_library
  >  (archive_name flags)
  >  (language asm)
  >  (names flags)
  >  (flags :standard (:include asm_flags.sexp)
  >   -DASM_VALUE=%{env:ASM_VALUE=42}))
  > EOF
  $ dune build flags.o

The same flags field works for assembly stubs.

  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (c_flags -DC_ONLY)
  >   (cxx_flags -DCXX_ONLY)))
  > (rule
  >  (target asm_flags.sexp)
  >  (action (write-file %{target} "(-DASM_FLAG)")))
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (language asm)
  >   (names flags)
  >   (flags :standard (:include asm_flags.sexp)
  >    -DASM_VALUE=%{env:ASM_VALUE=42})))
  > EOF
  $ touch main.ml
  $ dune build flags.o
  $ cd ..

The assembly language is versioned.

  $ make_dune_project 3.25
  $ dune build
  File "dune", line 3, characters 11-14:
  3 |  (language asm)
                 ^^^
  Error: 'asm' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]

Use a mock MSVC toolchain to check assembler selection on any host. The mock
compilers write their arguments to the object file instead of assembling it.
Dune must invoke ml64 with MASM options, not the C compiler.

  $ mkdir msvc
  $ cd msvc
  $ make_dune_project 3.26
  $ actual_ocamlc=$(command -v ocamlc)
  $ cat > ocamlc-msvc <<EOF
  > #!/bin/sh
  > if [ "\$1" = "-config" ]; then
  >   "$actual_ocamlc" -config | sed \
  >     -e 's/^ccomp_type:.*/ccomp_type: msvc/' \
  >     -e 's/^architecture:.*/architecture: amd64/' \
  >     -e 's/^os_type:.*/os_type: Win32/' \
  >     -e 's/^system:.*/system: win64/' \
  >     -e 's/^ext_obj:.*/ext_obj: .obj/' \
  >     -e 's|^c_compiler:.*|c_compiler: $PWD/c-compiler|'
  > else
  >   exec "$actual_ocamlc" "\$@"
  > fi
  > EOF
  $ cat > findlib.conf <<EOF
  > path=""
  > ocamlc="$PWD/ocamlc-msvc"
  > EOF
  $ cat > c-compiler <<'EOF'
  > #!/bin/sh
  > case "$1" in
  >   -c) output=$3 ;;
  >   /nologo) output=${3#/Fo} ;;
  >   *) exit 1 ;;
  > esac
  > printf '%s\n' "$(basename "$0")" "$@" > "$output"
  > EOF
  $ cp c-compiler ml64
  $ chmod +x ocamlc-msvc c-compiler ml64
  $ unset OCAMLFIND_TOOLCHAIN
  $ export OCAMLFIND_CONF="$PWD/findlib.conf"
  $ export PATH="$PWD:$PATH"
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name msvc)
  >  (language asm)
  >  (names "hello world")
  >  (flags :standard "/DASM_TEXT=hello world"))
  > EOF
  $ touch 'hello world.asm'
  $ dune build '"hello world.obj"'
  $ cat '_build/default/hello world.obj'
  ml64
  /nologo
  /quiet
  /Fohello world.obj
  /DASM_TEXT=hello world
  /c
  hello world.asm

MinGW still uses the C compiler driver, despite targeting Windows.

  $ sed \
  >   -e 's/ccomp_type: msvc/ccomp_type: cc/' \
  >   -e 's/system: win64/system: mingw64/' \
  >   -e 's/ext_obj: .obj/ext_obj: .o/' \
  >   ocamlc-msvc > ocamlc-mingw
  $ chmod +x ocamlc-mingw
  $ sed 's/ocamlc-msvc/ocamlc-mingw/' findlib.conf > mingw.conf
  $ export OCAMLFIND_CONF="$PWD/mingw.conf"
  $ mv 'hello world.asm' 'hello world.S'
  $ sed 's|/DASM_TEXT|-DASM_TEXT|' dune > dune.mingw
  $ mv dune.mingw dune
  $ dune build '"hello world.o"'
  $ cat '_build/default/hello world.o'
  c-compiler
  -c
  -o
  hello world.o
  -DASM_TEXT=hello world
  hello world.S

Do not silently assemble for x86-64 when targeting a different architecture.

  $ sed 's/architecture: amd64/architecture: arm64/' ocamlc-msvc > ocamlc-arm
  $ chmod +x ocamlc-arm
  $ sed 's/ocamlc-msvc/ocamlc-arm/' findlib.conf > arm.conf
  $ export OCAMLFIND_CONF="$PWD/arm.conf"
  $ dune build '"hello world.obj"'
  File "dune", line 4, characters 8-21:
  4 |  (names "hello world")
              ^^^^^^^^^^^^^
  Error: MSVC assembly requires x86-64, not arm64.
  [1]
