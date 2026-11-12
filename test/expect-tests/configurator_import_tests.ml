open Stdune

let%expect_test "Configurator preserves process bytes and normalizes CRLF logs" =
  let raw_preserved, log_normalized =
    if Sys.win32
    then true, true
    else (
      let logs = ref [] in
      let context =
        Configurator.V1.create ~log:(fun line -> logs := line :: !logs) "line-endings"
      in
      logs := [];
      let { Configurator.V1.Process.stdout; stderr; exit_code } =
        Configurator.V1.Process.run
          context
          "sh"
          [ "-c"
          ; "printf 'one\\r\\n\\r\\ntwo\\nembedded\\rcarriage\\r\\n'; printf '\\r\\nend' \
             >&2"
          ]
      in
      let lines =
        List.rev !logs |> List.filter ~f:(Base.String.is_prefix ~prefix:" | ")
      in
      ( exit_code = 0
        && String.equal stdout "one\r\n\r\ntwo\nembedded\rcarriage\r\n"
        && String.equal stderr "\r\nend"
      , List.equal
          String.equal
          lines
          [ " | one"; " | "; " | two"; " | embedded\rcarriage"; " | "; " | end" ] ))
  in
  printfn "raw stdout/stderr preserved: %b" raw_preserved;
  printfn "normalized log lines: %b" log_normalized;
  [%expect
    {|
    raw stdout/stderr preserved: true
    normalized log lines: true |}]
;;
