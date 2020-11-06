open! Dune_engine

(* This module simply expands a [(library ... (ctypes ...))] stanza into the
   set of [library], [rule] and [action] stanzas and .ml files needed to
   more conveniently build OCaml bindings for C libraries.  Aside from perhaps
   providing a '#include "header.h"' line, you should be able to wrap an
   entire C library without writing a single line of C code.

   The result of this stanza is a single library you can reference from your
   projects to get at the underlying C types/data/functions that have been
   exposed.

   All you have to do is configure the stanza and provide two ocaml modules
   - the types/data wrapping module
   - the functions wrapping module

   This module will then, behind the scenes
   - generate a types/constants generator
   - generate a functions generator
   - set up a discovery program to query pkg-config for compile and link flags
   - use the types/data and functions modules you filled in to tie everything
   together into a neat library
*)

let gen_rule ~targets ~action () =
  let open Dune_file in
  { Rule.targets; action }

let gen_library ?wrapped ?foreign_stubs ?c_library_flags ~name
      ~public_name ~modules ~libraries () =
  let open Dune_file in
  { Library.name; public_name; modules; libraries; wrapped; foreign_stubs }

let gen_executable ~name ~modules ~libraries () =
  let open Dune_file in
  { Executable.name; modules; libraries }

(* It may help to understand what this generator function is trying to do by
   having a look at the hand-written version it's replacing.
   XXX: link to mpg123 dune file *)
let really_expand lib ctypes =
  let open Dune_file in
  [ gen_executable
      ~name:"mpg123_discover"
      ~libraries:["dune.configurator"]
      ()
  ; gen_rule
      ~targets:["c_flags.sexp"; "c_flags.txt"; "c_library_flags.sexp"]
      ~action:["run discover.exe"]
      ()
  ; gen_library
      ~name:("mpg123_c_type_descriptions")
      ~public_name:("mpg123.c_type_descriptions")
      ~modules:"Mpg123_c_type_descriptions"
      ~libraries:["ctypes"]
      ()
  ; gen_executable
      ~name:"type_gen"
      ~modules:"Type_gen"
      ~libraries:["ctypes.stubs"; "ctypes.foreign"; "mpg123_c_type_descriptions"]
      ()
  ; gen_rule_stdout
      ~with_stdout_to:"c_generated_types.c"
      ~run:("./type_gen.exe")
      ()
  ; gen_rule
      ~targets:["c_generated_types.exe"]
      ~deps:[":c c_generated_types.c"]
      ~action:["system blah blah"]
      ()
  ; gen_rule_stdout
      ~with_stdout_to:"mpg123_c_generated_types.ml"
      ~run:("./c_generated_types.exe")
      ()
  ; gen_library
      ~name:"mpg123_c_function_descriptions"
      ~public_name:"mpg123.c_function_descriptions"
      ~modules:["Mpg123_c_generated_types"; "Mpg123_c_function_descriptions";
                "Mpg123_c_types"]
      ~wrapped:false
      ~flags:":standard -w -27 -w -9"
      ~libraries:["ctypes"; "mpg123_c_type_descriptions"]
      ()
  ; gen_executable
      ~name:"function_gen"
      ~modules:"Function_gen"
      ~libraries:["ctypes.stubs"; "mpg123_c_function_descriptions"]
      ()
  ; gen_rule_stdout
      ~with_stdout_to:"c_generated_functions.c"
      ~run:"./function_gen.exe c mpg123_stub"
      ()
  ; gen_rule_stdout
      ~with_stdout_to:"mpg123_c_generated_functions.ml"
      ~run:"./function_gen.exe ml mpg123_stub"
      ()
  ; gen_library
      ~name:"mpg123_c"
      ~public_name:"mpg123.c"
      ~libraries:["ctypes"; "mpg123_c_function_descriptions"]
      ~modules:["Mpg123_c"; "Mpg123_c_generated_functions"]
      ~foreign_stubs:[
        ("language" , "c");
        ("names"    , "c_generated_functions");
        ("flags"    , ":include c_flags.sexp")
      ]
      ~c_library_flags:":include c_library_flags.sexp"
      ()
  ]

let expand = function
  | Dune_file.Library lib ->
    begin match lib.Dune_file.Library.ctypes with
    | Some ctypes -> really_expand lib ctypes
    | None -> assert false
    end
  | _ -> assert false
