open Dune_lang
open Dune_config_file

let () = Printexc.record_backtrace false

let parse s =
  let ast = Parser.parse_string ~fname:"expect_test" ~mode:Parser.Mode.Single s in
  let decode =
    Dune_lang.Syntax.set Dune_lang.Stanza.syntax (Active (3, 0)) Dune_config.decode
  in
  Dune_lang.Decoder.parse decode Stdune.Univ_map.empty ast
  |> Dune_config.(superpose default)
  |> Dune_config.to_dyn
  |> Dune_tests_common.print_dyn
;;

let%expect_test "cache-check-probability 0.1" =
  parse "(cache-check-probability 0.1)";
  [%expect
    {|
    { display = Simple { verbosity = Quiet; status_line = false }
    ; concurrency = Fixed 1
    ; terminal_persistence = Clear_on_rebuild
    ; sandboxing_preference = []
    ; cache_enabled = Disabled
    ; cache_reproducibility_check = Check_with_probability 0.1
    ; cache_storage_mode = None
    ; action_stdout_on_success = Print
    ; action_stderr_on_success = Print
    ; experimental = []
    }
 |}]
;;

let%expect_test "cache-storage-mode copy" =
  parse "(cache-storage-mode copy)";
  [%expect
    {|
    { display = Simple { verbosity = Quiet; status_line = false }
    ; concurrency = Fixed 1
    ; terminal_persistence = Clear_on_rebuild
    ; sandboxing_preference = []
    ; cache_enabled = Disabled
    ; cache_reproducibility_check = Skip
    ; cache_storage_mode = Some Copy
    ; action_stdout_on_success = Print
    ; action_stderr_on_success = Print
    ; experimental = []
    }
 |}]
;;

let%expect_test "cache-storage-mode hardlink" =
  parse "(cache-storage-mode hardlink)";
  [%expect
    {|
    { display = Simple { verbosity = Quiet; status_line = false }
    ; concurrency = Fixed 1
    ; terminal_persistence = Clear_on_rebuild
    ; sandboxing_preference = []
    ; cache_enabled = Disabled
    ; cache_reproducibility_check = Skip
    ; cache_storage_mode = Some Hardlink
    ; action_stdout_on_success = Print
    ; action_stderr_on_success = Print
    ; experimental = []
    }
 |}]
;;
