Define a helper ./dump.sh unction with offsets removed, and one byte per line in
hex (so that the output is compiler version / alignment independent).

  $ cat >./dump.sh <<'EOF'
  > set -eu
  > od -v -A n -t x1 $1 | tr ' ' '\n' | sed '/^$/d'
  > EOF
  $ chmod +x ./dump.sh
  $ cat >./compare.sh <<'EOF'
  > set -eu
  > ./dump.sh $1 >$1.dump
  > ./dump.sh $2 >$2.dump
  > cmp -l $1.dump $2.dump | wc -l | sed -e 's/^ *//'
  > EOF
  $ chmod +x compare.sh

A repro that builds and installs multiple binaries, and promotes a bytecode and
native executable in same rule (this is very likely to detect corruption with
shared buffer):

  $ git init --quiet
  $ git commit -m init --allow-empty --quiet
  $ git tag -a v0.0.1 -m v0.0.1
  $ echo "(lang dune 2.0)" > dune-project
  $ touch xapi-datamodel.opam
  $ cat >dune <<EOF
  > (executable
  >   (modes byte exe)
  >   (name gen_lifecycle)
  >   (public_name gen_lifecycle)
  >   (package xapi-datamodel)
  >   (modules gen_lifecycle)
  >   (libraries
  >     dune-build-info)
  >   (promote (until-clean)))
  > ; use the binary promoted file from the source dir (not the build dir) that has
  > ; the correct version number embedded
  > (rule
  >  (deps gen_lifecycle.exe)
  >  (action (with-stdout-to datamodel_lifecycle.ml.generated (system %{project_root}/gen_lifecycle.exe))))
  > (executables
  >  (modes exe)
  >  (public_names gen1)
  >  (modules gen gen1)
  >  (libraries dune-build-info))
  > (executables
  >  (modes byte)
  >  (public_names gen2)
  >  (modules genb gen2)
  >  (libraries dune-build-info))
  > EOF

  $ cat >gen_lifecycle.ml <<EOF
  > module Xapi_version = struct
  >   let version, xapi_version_major, xapi_version_minor =
  >     match Build_info.V1.version () with
  >     | None -> ("0.0.0", 0, 0)
  >     | Some v -> (
  >         let str = Build_info.V1.Version.to_string v in
  >         let version =
  >           if String.starts_with ~prefix:"v" str then
  >             String.sub str 1 (String.length str - 1)
  >           else str
  >         in
  >         try
  >           let maj, min =
  >             Scanf.sscanf version "%d.%d.%s" (fun maj min _rest -> (maj, min))
  >           in
  >           (version, maj, min)
  >         with _ ->
  >           failwith
  >             (Printf.sprintf
  >                "Couldn't determine xapi version - got unexpected version from \
  >                 dune: '%s'"
  >                version))
  > end
  > let current_version =
  >   print_endline Xapi_version.version;
  >   print_endline Xapi_version.version;
  >   print_endline Xapi_version.version;
  >   print_endline Xapi_version.version;
  >   Scanf.sscanf Xapi_version.version "%d.%d.%d%[.-]%s"
  >     (fun mj mn mc _sep _rest -> Printf.sprintf "%d.%d.%d" mj mn mc)
  > EOF

  $ cp gen_lifecycle.ml gen.ml
  $ cp gen_lifecycle.ml genb.ml
  $ cat >gen1.ml <<EOF
  > let ver_$i = Gen.current_version
  > let () = prerr_endline "some string"
  > EOF
  $ cat >gen2.ml <<EOF
  > let ver_$i = Genb.current_version
  > let () = prerr_endline "some string2"
  > EOF

  $ rm -f gen_lifecycle.bc gen_lifecycle.exe && dune clean && dune build && ./gen_lifecycle.exe >/dev/null
  Segmentation fault
  [139]
  $ cp _build/default/gen_lifecycle.exe gen_lifecycle.old

  $ dune install -j16 --prefix=./_install 2>/dev/null
  $ ./compare.sh _build/default/gen1.exe _install/bin/gen1
  100

  $ ./compare.sh _build/default/gen2.bc _install/bin/gen2
  100

  $ dune build --debug-artifact-substitution
  Found placeholder in _build/default/gen_lifecycle.exe:
  - placeholder: Vcs_describe "."
  - evaluates to: "v0.0.1"
  Found placeholder in _build/default/gen_lifecycle.bc:
  - placeholder: Vcs_describe "."
  - evaluates to: "v0.0.1"

  $ ./compare.sh gen_lifecycle.old ./gen_lifecycle.exe
  28362

  $ ./gen_lifecycle.exe
  Segmentation fault
  [139]
