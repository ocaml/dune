PPX drivers need the compilation target when the same source is compiled for
OCaml and Melange. Dune supplies the compilation-mode cookie for both modes
starting with language version 3.26.

  $ make_melange_project 3.26 1.0
  $ mkdir ppx
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name mode_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/mode_ppx.ml <<'EOF'
  > open Ppxlib
  > let mode = ref "absent"
  > let report_lint = ref false
  > let () =
  >   Driver.Cookies.add_handler (fun cookies ->
  >     mode :=
  >       Option.value ~default:"absent"
  >         (Driver.Cookies.get cookies "compilation-mode"
  >            Ast_pattern.(estring __)))
  > let selected ~loc name =
  >   let module_ =
  >     match !mode with
  >     | "melange" -> "Target_melange"
  >     | "ocaml" -> "Target_ocaml"
  >     | "absent" -> "Target_absent"
  >     | other -> Location.raise_errorf ~loc "Unexpected mode: %s" other
  >   in
  >   { txt = Longident.Ldot (Longident.Lident module_, name); loc }
  > let expression name field =
  >   Extension.V3.declare name Extension.Context.expression
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.pexp_ident ~loc (selected ~loc field))
  > let type_ =
  >   Extension.V3.declare "compilation_mode" Extension.Context.core_type
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.ptyp_constr ~loc (selected ~loc "t") [])
  > let lint kind _ =
  >   if !report_lint then Printf.eprintf "lint %s: %s\n" kind !mode;
  >   []
  > let () =
  >   Driver.add_arg "--report-lint" (Arg.Set report_lint) ~doc:"Report mode";
  >   Driver.register_transformation "mode"
  >     ~lint_impl:(lint "implementation") ~lint_intf:(lint "interface")
  >     ~rules:
  >       (List.map Context_free.Rule.extension
  >          [ expression "compilation_mode" "value"
  >          ; expression "compilation_mode_to_string" "to_string"
  >          ; type_
  >          ])
  > EOF

The implementation and interface select different types for the two targets.
The generated references must also be found during dependency analysis.

  $ make_case () {
  >   mkdir "$1"
  >   cat >"$1/dune" <<EOF
  > (library
  >  (name $1_shared)
  >  (wrapped false)
  >  (modules shared target_absent target_ocaml target_melange)
  >  (modes :standard melange)
  >  (preprocess ($2 mode_ppx)))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries $1_shared))
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (libraries $1_shared))
  > EOF
  >   cat >"$1/target_absent.ml" <<'EOF'
  > type t = int
  > let value = 0
  > let to_string (_ : t) = "absent"
  > EOF
  >   cat >"$1/target_ocaml.ml" <<'EOF'
  > type t = bool
  > let value = true
  > let to_string (_ : t) = "ocaml"
  > EOF
  >   cat >"$1/target_melange.ml" <<'EOF'
  > type t = string
  > let value = "melange"
  > let to_string (x : t) = x
  > EOF
  >   cat >"$1/shared.ml" <<'EOF'
  > let value = [%compilation_mode]
  > let label = [%compilation_mode_to_string] value
  > EOF
  >   cat >"$1/shared.mli" <<'EOF'
  > val value : [%compilation_mode]
  > val label : string
  > EOF
  >   echo 'let () = print_endline Shared.label' >"$1/main.ml"
  > }
  $ make_case ordinary pps
  $ make_case staged staged_pps

The staged library's JS emission scans the source for dependencies. Give that
scan explicit implementation references; the interface still needs PPX to
discover its dependency.

  $ cat >>staged/shared.ml <<'EOF'
  > let () =
  >   ignore Target_absent.value;
  >   ignore Target_ocaml.value;
  >   ignore Target_melange.value
  > EOF
  $ dune build ordinary/main.exe @ordinary/melange \
  >   staged/main.exe @staged/melange
  $ _build/default/ordinary/main.exe
  ocaml
  $ node _build/default/ordinary/out/ordinary/main.js
  melange
  $ _build/default/staged/main.exe
  ocaml
  $ node _build/default/staged/out/staged/main.js
  melange

The cookie is also supplied when preprocessing a melange.emit stanza directly.

  $ mkdir emit
  $ cp ordinary/*.ml ordinary/shared.mli emit/
  $ cat >emit/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (preprocess (pps mode_ppx)))
  > EOF
  $ dune build @emit/melange
  $ node _build/default/emit/out/emit/main.js
  melange

Lint drivers need the same information for both implementations and
interfaces.

  $ mkdir linted
  $ cat >linted/dune <<'EOF'
  > (library
  >  (name linted)
  >  (modes melange)
  >  (lint (pps mode_ppx -- --report-lint)))
  > EOF
  $ touch linted/linted.ml linted/linted.mli
  $ dune build @linted/lint 2>&1 | sort
  lint implementation: melange
  lint interface: melange

  $ mkdir native_linted
  $ cat >native_linted/dune <<'EOF'
  > (library
  >  (name native_linted)
  >  (lint (pps mode_ppx -- --report-lint)))
  > EOF
  $ touch native_linted/native_linted.ml native_linted/native_linted.mli
  $ dune build @native_linted/lint 2>&1 | sort
  lint implementation: ocaml
  lint interface: ocaml

Merlin uses a separate PPX invocation. Its configuration must retain the
compilation target for both OCaml and Melange modules.

  $ mkdir editor
  $ cat >editor/dune <<'EOF'
  > (library
  >  (name editor)
  >  (modes melange)
  >  (preprocess (pps mode_ppx)))
  > EOF
  $ touch editor/editor.ml
  $ dune build editor/.merlin-conf/lib-editor \
  >   ordinary/.merlin-conf/lib-ordinary_shared
  $ merlin_mode () {
  >   dune ocaml merlin dump-config --format=json "$1" | jq -r '
  >     .[] | select(.source_path | endswith("/" + $source))
  >     | select(any(.config[];
  >         .[0] == "B" and (.[1] | endswith($obj_dir))))
  >     | .config[] | select(.[0] == "FLG" and .[1][0] == "-ppx")
  >     | .[1][1]
  >     | if contains("compilation-mode")
  >       then capture("compilation-mode=\"(?<mode>[^\"]+)\"").mode
  >       else "absent" end' --arg source "$2" --arg obj_dir "$3" | sort -u
  > }
  $ merlin_mode "$PWD/editor" editor.ml .editor.objs/melange
  melange
  $ merlin_mode "$PWD/ordinary" ordinary/shared.ml .ordinary_shared.objs/byte
  ocaml
  $ merlin_mode "$PWD/ordinary" ordinary/shared.ml .ordinary_shared.objs/melange
  melange

Projects using Dune 3.25 retain the existing cookie behavior.

  $ make_melange_project 3.25 1.0
  $ dune build ordinary/main.exe @ordinary/melange \
  >   staged/main.exe @staged/melange
  $ _build/default/ordinary/main.exe
  absent
  $ node _build/default/ordinary/out/ordinary/main.js
  absent
  $ _build/default/staged/main.exe
  absent
  $ node _build/default/staged/out/staged/main.js
  absent
  $ echo >>linted/linted.ml
  $ echo >>linted/linted.mli
  $ dune build @linted/lint 2>&1 | sort
  lint implementation: absent
  lint interface: absent
  $ echo >>native_linted/native_linted.ml
  $ echo >>native_linted/native_linted.mli
  $ dune build @native_linted/lint 2>&1 | sort
  lint implementation: absent
  lint interface: absent
  $ dune build editor/.merlin-conf/lib-editor
  $ merlin_mode "$PWD/editor" editor.ml .editor.objs/melange
  absent
  $ merlin_mode "$PWD/ordinary" ordinary/shared.ml .ordinary_shared.objs/byte
  absent

A PPX library may request the same cookie value as Dune. Its request also
applies to older projects, where Dune does not add the cookie.

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name mode_ppx)
  >  (kind (ppx_rewriter (cookies (compilation-mode melange))))
  >  (libraries ppxlib))
  > EOF
  $ dune build @emit/melange
  $ node _build/default/emit/out/emit/main.js
  melange
  $ make_melange_project 3.26 1.0
  $ dune build @emit/melange
  $ node _build/default/emit/out/emit/main.js
  melange

A conflicting library request is rejected when Dune supplies the cookie.

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name mode_ppx)
  >  (kind (ppx_rewriter (cookies (compilation-mode absent))))
  >  (libraries ppxlib))
  > EOF
  $ dune build @emit/melange
  File "emit/dune", line 3, characters 13-27:
  3 |  (preprocess (pps mode_ppx)))
                   ^^^^^^^^^^^^^^
  Error: dune and mode_ppx have inconsistent requests for cookie
  "compilation-mode"; dune requests "melange" and mode_ppx requests "absent"
  [1]

Conflicting requests are also rejected for OCaml.

  $ mkdir native
  $ cp ordinary/*.ml ordinary/shared.mli native/
  $ cat >native/dune <<'EOF'
  > (executable
  >  (name main)
  >  (preprocess (pps mode_ppx)))
  > EOF
  $ dune build native/main.exe
  File "native/dune", line 3, characters 13-27:
  3 |  (preprocess (pps mode_ppx)))
                   ^^^^^^^^^^^^^^
  Error: dune and mode_ppx have inconsistent requests for cookie
  "compilation-mode"; dune requests "ocaml" and mode_ppx requests "absent"
  [1]

Matching requests are accepted for OCaml.

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name mode_ppx)
  >  (kind (ppx_rewriter (cookies (compilation-mode ocaml))))
  >  (libraries ppxlib))
  > EOF
  $ dune build native/main.exe
  $ _build/default/native/main.exe
  ocaml

Before Dune 3.26 the library's cookie is used for either target.

  $ make_melange_project 3.25 1.0
  $ dune build native/main.exe @emit/melange
  $ _build/default/native/main.exe
  ocaml
  $ node _build/default/emit/out/emit/main.js
  ocaml
