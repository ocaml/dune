open Dune_lang
open Dune_tests_common

let parse s =
  let ast =
    Parser.parse_string ~fname:"expect_test" ~mode:Parser.Mode.Single s
  in
  Dune_lang.Decoder.parse Dune.Config.decode Stdune.Univ_map.empty ast
  |> Dune.Config.to_dyn |> print_dyn

let%expect_test _ =
  parse "(cache-trim-period 2m)";
  [%expect
    {|
{ display = "quiet"
; concurrency = "1"
; terminal_persistence = "preserve"
; sandboxing_preference = []
; cache_mode = "disabled"
; cache_transport = "daemon"
; cache_check_probability = 0.
; cache_trim_period = 120
; cache_trim_size = 10000000000
}
 |}]

let%expect_test _ =
  parse "(cache-trim-period 2)";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "File\
   \n\"expect_test\",\
   \nline\
   \n1,\
   \ncharacters\
   \n19-20:\
   \nError: missing suffix, use one of s, m, h\
   \n") |}]

let%expect_test _ =
  parse "(cache-trim-period 2k)";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "File\
   \n\"expect_test\",\
   \nline\
   \n1,\
   \ncharacters\
   \n19-21:\
   \nError: invalid suffix, use one of s, m, h\
   \n") |}]

let%expect_test _ =
  parse "(cache-trim-size 2kB)";
  [%expect
    {|
{ display = "quiet"
; concurrency = "1"
; terminal_persistence = "preserve"
; sandboxing_preference = []
; cache_mode = "disabled"
; cache_transport = "daemon"
; cache_check_probability = 0.
; cache_trim_period = 600
; cache_trim_size = 2000
}
 |}]

let%expect_test _ =
  parse "(cache-trim-size 42)";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "File\
   \n\"expect_test\",\
   \nline\
   \n1,\
   \ncharacters\
   \n17-19:\
   \nError: missing suffix, use one of B, kB, KB, MB, GB\
   \n") |}]
