let flags =
  if (not Sys.win32) || Array.length Sys.argv < 2
  then "()"
  else
    (if Sys.argv.(1) = "msvc"
     then
       [ "/wd5287"; "/wd4333"; "/wd4172"; "/wd4146"; "/wd4244" ]
       |> List.map (Printf.sprintf "%S")
     else
       [ "-Wno-return-local-addr"
       ; "-Wno-unused-label"
       ; "-Wno-unused-but-set-variable"
       ; "-Wno-format"
       ])
    |> String.concat " "
    |> Printf.sprintf "(%s)"
;;

let () = print_endline flags
