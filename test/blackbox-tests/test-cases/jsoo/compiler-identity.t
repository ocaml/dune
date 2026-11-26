Compiler selection matters without package management. Two executables
share a library but select different compilers through ordinary env bindings.
The fake compilers report identical build configurations and reject intermediate
files produced by the other compiler, including shapes and shared runtimes.

  $ make_dune_project 3.25
  $ mkdir tools lib a b
  $ cat > dune <<'EOF'
  > (env (_ (binaries (tools/first.exe as js_of_ocaml))))
  > EOF
  $ echo '(executables (names first second))' > tools/dune
  $ echo 'let () = Driver.main "first"' > tools/first.ml
  $ echo 'let () = Driver.main "second"' > tools/second.ml
  $ cat > tools/driver.ml <<'EOF'
  > let main marker =
  >   let args = List.tl (Array.to_list Sys.argv) in
  >   match args with
  >   | ["--version"] -> print_endline "6.4.1"
  >   | "--build-config" :: _ -> print_endline "effects=disabled"
  >   | _ ->
  >     let rec output = function
  >       | "-o" :: path :: _ -> path
  >       | _ :: rest -> output rest
  >       | [] -> failwith "missing output"
  >     in
  >     let dst = output args in
  >     List.iter (fun path ->
  >       if path <> dst && List.exists (Filename.check_suffix path)
  >         [".cma.js"; ".cmo.js"; ".bc.runtime.js"] then (
  >         let input = open_in path in
  >         let actual = input_line input in
  >         close_in input;
  >         if actual <> marker then (
  >           prerr_endline ("compiler mismatch: " ^ marker);
  >           exit 1))) args;
  >     let rec check_config = function
  >       | "--apply-build-config" :: value :: rest ->
  >         if value <> "effects=disabled" then failwith "bad build config";
  >         check_config rest
  >       | _ :: rest -> check_config rest
  >       | [] -> ()
  >     in
  >     check_config args;
  >     let out = open_out dst in
  >     output_string out (marker ^ "\n");
  >     close_out out
  > EOF
  $ cat > lib/dune <<'EOF'
  > (library (name shared) (js_of_ocaml (javascript_files runtime.js)))
  > EOF
  $ echo 'let value = 42' > lib/shared.ml
  $ echo '/* shared runtime */' > lib/runtime.js
  $ echo '(executable (name main) (modes js) (libraries shared))' > a/dune
  $ echo 'let () = print_int Shared.value' > a/main.ml
  $ cp a/main.ml b/main.ml
  $ cat > b/dune <<'EOF'
  > (env (_ (binaries (../tools/second.exe as js_of_ocaml))))
  > (executable (name main) (modes js) (libraries shared))
  > EOF
  $ dune build a/main.bc.js
  $ cat _build/default/a/main.bc.js
  first

The consumers get distinct shared objects despite identical build-config
strings. This covers library archives, standard-library shapes, and runtimes.

  $ dune build a/main.bc.js b/main.bc.js
  $ cat _build/default/a/main.bc.js _build/default/b/main.bc.js
  first
  second
  $ find _build/default/lib/.shared.objs/jsoo -name shared.cma.js -exec cat {} \; | sort
  first
  second
  $ find _build/default/.js -name stdlib.cma.js -exec cat {} \; | sort
  first
  second
  $ find _build/default/.js -name runtime.bc.runtime.js -exec cat {} \; | sort
  first
  second

Changing the binding at the same path must invalidate compilation, even if the
compiler version and build configuration do not change.

  $ cat > b/dune <<'EOF'
  > (env (_ (binaries (../tools/first.exe as js_of_ocaml))))
  > (executable (name main) (modes js) (libraries shared))
  > EOF
  $ dune build b/main.bc.js a/main.bc.js
  $ cat _build/default/b/main.bc.js
  first

The compiler remains a host tool when JS artifacts are produced in a target
context. Each consumer retains its host-side binding for shared compilation.

  $ cat > b/dune <<'EOF'
  > (env (_ (binaries (../tools/second.exe as js_of_ocaml))))
  > (executable (name main) (modes js) (libraries shared))
  > EOF
  $ make_host_target_workspace
  $ dune build _build/target/a/main.bc.js _build/target/b/main.bc.js
  $ cat _build/target/a/main.bc.js _build/target/b/main.bc.js
  first
  second
  $ dune trace cat | jq_dune -r '
  >   processes | select(.args.prog | endswith("js_of_ocaml"))
  >   | .args.prog | sub("^.*host/"; "host/")' | sort -u
  host/.bin/js_of_ocaml
  host/b/.bin/js_of_ocaml
