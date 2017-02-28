open Import

let system_shell =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg, None)
    | None ->
      (Path.absolute ("/" ^ cmd),
       arg,
       Some { fail = fun () ->
         die "I need %s to %s but I couldn't find it :(\n\
              Who doesn't have %s%s??!"
           cmd needed_to cmd os })
