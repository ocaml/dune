The bootstrap launcher must invoke the secondary compiler on Windows, including
when compiler paths, temporary paths, and forwarded arguments contain spaces.
Use native executables so the launcher's commands go through the platform shell.

  $ mkdir boot tools "temp dir" "secondary compiler"
  $ mkdir "secondary compiler/bin"
  $ cat > tool.ml <<'EOF'
  > let () =
  >   let root = Sys.getcwd () in
  >   let secondary = Filename.concat root "secondary compiler" in
  >   match Filename.basename Sys.argv.(0) with
  >   | "ocamlfind" | "ocamlfind.exe" ->
  >     print_endline (Filename.concat secondary "bin")
  >   | "ocamllex" | "ocamllex.exe" -> ()
  >   | "ocaml" | "ocaml.exe" ->
  >     (match List.rev (List.tl (Array.to_list Sys.argv)) with
  >      | "build dir" :: "--boot-dir" :: "--secondary" :: script :: "unix.cma"
  >        :: _ ->
  >        if not (Sys.file_exists script) then failwith "missing bootstrap script";
  >        print_endline "secondary compiler invoked"
  >      | _ -> failwith "incorrect bootstrap arguments")
  >   | _ ->
  >     if Array.length Sys.argv = 1 then print_endline root
  >     else print_endline (if Sys.win32 then ".exe" else "")
  > EOF
  $ ocamlopt -o tool.exe tool.ml
  $ exe=$(./tool.exe extension | tr -d '\r')
  $ cp tool.exe "tools/ocamlfind$exe"
  $ cp tool.exe "secondary compiler/bin/ocaml$exe"
  $ cp tool.exe "secondary compiler/bin/ocamllex$exe"
  $ export PATH="$PWD/tools:$PATH"
  $ native_root=$(./tool.exe | tr -d '\r')
  $ export TMPDIR="$native_root/temp dir"
  $ export TEMP="$TMPDIR"
  $ export TMP="$TMPDIR"

Exercise the secondary path without requiring an old compiler in the test switch.

  $ cat > bootstrap.ml <<'EOF'
  > module Sys = struct
  >   include Sys
  >   let ocaml_version = "4.13.1"
  > end
  > EOF
  $ cat ../../../../boot/bootstrap.ml >> bootstrap.ml
  $ if ocaml bootstrap.ml --boot-dir "build dir" >bootstrap.log 2>&1; then
  >   tr -d '\r' <bootstrap.log | grep '^secondary compiler invoked$'
  > else
  >   echo 'bootstrap failed'
  > fi
  bootstrap failed
