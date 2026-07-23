open Dune_tests_common
open Stdune
module Cc_flags = Dune_rules.For_tests.Cc_flags

let () = init ()

(* Use a modern OCaml version so the C++ [-std] flag is included. *)
let version = Ocaml.Version.make (5, 0, 0)

let show = function
  | [] -> "(none)"
  | flags -> String.concat ~sep:" " flags
;;

let flags_of word =
  let vendor = Cc_flags.parse_cc_vendor word in
  Printf.sprintf
    "compile=[%s] link=[%s] warnings=[%s] color=[%s]"
    (show (Cc_flags.base_cxx_compile_flags version vendor))
    (show (Cc_flags.base_cxx_link_flags vendor))
    (show (Cc_flags.warnings vendor))
    (show (Cc_flags.fdiagnostics_color vendor))
;;

let%expect_test "vendor identifiers route to the right flag families" =
  List.iter
    [ "msvc"; "icc"; "mingw"; "clang"; "gcc"; "xlc"; "sunc"; "unknown" ]
    ~f:(fun word -> Printf.printf "%-8s %s\n" word (flags_of word));
  [%expect
    {|
    msvc     compile=[/TP] link=[(none)] warnings=[-W2] color=[(none)]
    icc      compile=[-x c++ -std=gnu++11] link=[-lstdc++ -shared-libgcc] warnings=[-Wall] color=[-fdiagnostics-color=always]
    mingw    compile=[-x c++ -std=gnu++11] link=[-lstdc++ -shared-libgcc] warnings=[-Wall] color=[-fdiagnostics-color=always]
    clang    compile=[-x c++ -std=gnu++11] link=[-lc++] warnings=[-Wall] color=[-fdiagnostics-color=always]
    gcc      compile=[-x c++ -std=gnu++11] link=[-lstdc++ -shared-libgcc] warnings=[-Wall] color=[-fdiagnostics-color=always]
    xlc      compile=[(none)] link=[(none)] warnings=[(none)] color=[(none)]
    sunc     compile=[(none)] link=[(none)] warnings=[(none)] color=[(none)]
    unknown  compile=[(none)] link=[(none)] warnings=[(none)] color=[(none)]
    |}]
;;

let%expect_test "unrecognized words and surrounding whitespace are handled" =
  (* Leading/trailing whitespace from the probe output is trimmed. *)
  Printf.printf "whitespace: %s\n" (flags_of "  gcc\n");
  (* An unknown word falls through to the neutral family (and would warn). *)
  Printf.printf "unknown:    %s\n" (flags_of "borland");
  [%expect
    {|
    whitespace: compile=[-x c++ -std=gnu++11] link=[-lstdc++ -shared-libgcc] warnings=[-Wall] color=[-fdiagnostics-color=always]
    unknown:    compile=[(none)] link=[(none)] warnings=[(none)] color=[(none)]
    |}]
;;
