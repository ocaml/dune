Observe runtime settings in native OCaml stand-ins for odoc and js_of_ocaml.
The wrappers can hide the runtime capability report to model an older tool,
independently of the compiler version reported to Dune.

  $ make_dune_project 3.25
  $ unset OCAMLRUNPARAM CAMLRUNPARAM
  $ touch probe.opam
  $ mkdir bin native
  $ export TOOL_RUNTIME_LOG="$PWD/tools.log"
  $ cat > native/tool.ml <<'EOF'
  > let () =
  >   let tool = Sys.getenv "TOOL_NAME" in
  >   let args = Array.to_list Sys.argv |> List.tl in
  >   if args = ["--version"] then
  >     print_endline (if tool = "odoc" then "2.4.4" else "6.0.0")
  >   else if tool = "odoc" && List.hd args = "compile-deps" then ()
  >   else begin
  >     let get var =
  >       match Sys.getenv_opt var with None -> "unset" | Some v -> v
  >     in
  >     Printf.printf "%s OCAMLRUNPARAM=%s CAMLRUNPARAM=%s MANPATH=%s\n"
  >       tool (get "OCAMLRUNPARAM") (get "CAMLRUNPARAM") (get "MANPATH");
  >     let rec output = function
  >       | "-o" :: path :: _ -> close_out (open_out path)
  >       | _ :: rest -> output rest
  >       | [] -> ()
  >     in
  >     output args
  >   end
  > EOF
  $ ocamlopt -o native/tool.exe native/tool.ml
  $ export TOOL_RUNTIME_EXE="$PWD/native/tool.exe"
  $ cat > bin/wrapper <<'EOF'
  > #!/bin/sh
  > export TOOL_NAME=${0##*/}
  > if [ "${HIDE_RUNTIME_SUPPORT-}" = yes ] &&
  >    [ "${OCAMLRUNPARAM-}" = Xhelp ]; then
  >   unset OCAMLRUNPARAM
  > fi
  > exec "$TOOL_RUNTIME_EXE" "$@"
  > EOF
  $ chmod +x bin/wrapper
  $ ln -s wrapper bin/odoc
  $ ln -s wrapper bin/js_of_ocaml
  $ export PATH="$PWD/bin:$PATH"
  $ cat > dune <<'EOF'
  > (library (name probe) (public_name probe) (modules probe))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries probe)
  >  (modes byte js))
  > (env (_ (env-vars (MANPATH /tool-runtime-manpath))))
  > (rule
  >  (alias runtest)
  >  (action
  >   (setenv TOOL_NAME user (run %{env:TOOL_RUNTIME_EXE=}))))
  > EOF
  $ echo 'let message = "hello"' > probe.ml
  $ echo 'let () = print_endline Probe.message' > main.ml
  $ build () {
  >   build_dir="_build_$1"
  >   shift
  >   dune build --cache=disabled --build-dir "$build_dir" "$@" \
  >     main.bc.js .probe.objs/byte/probe.odoc > "$TOOL_RUNTIME_LOG" 2>&1 ||
  >     { cat "$TOOL_RUNTIME_LOG"; return 1; }
  >   grep ' OCAMLRUNPARAM=' "$TOOL_RUNTIME_LOG" | sort -u
  > }
  $ build defaults
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath

Action runners receive the same environment.

  $ build worker --action-runner
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath

Unrelated user actions are not tuned.

  $ dune build --build-dir _build_defaults @runtest
  user OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath

Unsupported tool runtimes do not receive unsupported settings.

  $ HIDE_RUNTIME_SUPPORT=yes build unsupported
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath

Explicit values, including empty values and the legacy variable, take priority.

  $ OCAMLRUNPARAM=o=42 build explicit
  js_of_ocaml OCAMLRUNPARAM=o=42 CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=o=42 CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  $ CAMLRUNPARAM=o=43 build legacy
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM=o=43 MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM=o=43 MANPATH=/tool-runtime-manpath
  $ OCAMLRUNPARAM= build empty
  js_of_ocaml OCAMLRUNPARAM= CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM= CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  $ CAMLRUNPARAM= build legacy_empty
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM= MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM= MANPATH=/tool-runtime-manpath

Directory settings are applied before choosing runtime defaults.

  $ sed -i.bak 's/(MANPATH /(CAMLRUNPARAM "") (MANPATH /' dune
  $ build directory_empty --action-runner
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM= MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM= MANPATH=/tool-runtime-manpath

A tool's runtime is independent of the compiler's reported version.

  $ sed -i.bak 's/(CAMLRUNPARAM "") //' dune
  $ export REAL_OCAML_BIN=$(dirname "$(command -v ocamlc)")
  $ cat > bin/compiler <<'EOF'
  > #!/bin/sh
  > if [ "$1" = -config ]; then
  >   "$REAL_OCAML_BIN/${0##*/}" "$@" | sed 's/^version:.*/version: 5.4.0/'
  > else
  >   exec "$REAL_OCAML_BIN/${0##*/}" "$@"
  > fi
  > EOF
  $ chmod +x bin/compiler
  $ for tool in "$REAL_OCAML_BIN"/ocaml*; do
  >   name=${tool##*/}
  >   case "$name" in
  >     ocamlc|ocamlc.opt) ln -s compiler "bin/$name" ;;
  >     *) ln -s "$tool" "bin/$name" ;;
  >   esac
  > done
  $ build older_compiler
  js_of_ocaml OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
  odoc OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MANPATH=/tool-runtime-manpath
