open Stdune
open Import

let files_to_promote ~common files : Diff_promotion.files_to_promote =
  match files with
  | [] -> All
  | _ ->
    let files =
      List.map files ~f:(fun fn ->
          Path.Source.of_string (Common.prefix_target common fn))
    in
    let on_missing fn =
      User_warning.emit
        [ Pp.textf "Nothing to promote for %s."
            (Path.Source.to_string_maybe_quoted fn)
        ]
    in
    These (files, on_missing)

module Promote = struct
  let term =
    let+ common = Common.term
    and+ files =
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE")
    in
    let _config = Common.init common in
    let files_to_promote = files_to_promote ~common files in
    Diff_promotion.promote_files_registered_in_last_run files_to_promote

  let command =
    let doc = "Promote files from the last run" in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          {|Considering all actions of the form $(b,(diff a b)) that failed
           in the last run of dune, $(b,dune promote) does the following:

           If $(b,a) is present in the source tree but $(b,b) isn't, $(b,b) is
           copied over to $(b,a) in the source tree. The idea behind this is that
           you might use $(b,(diff file.expected file.generated)) and then call
           $(b,dune promote) to promote the generated file.
         |}
      ; `Blocks Common.help_secs
      ]
    in
    Cmd.v (Cmd.info "promote" ~doc ~man) term
end

module Run = struct
  let info = Cmd.info "run"

  let command = Cmd.v info Promote.term
end

module Diff = struct
  let info = Cmd.info "diff"

  let term =
    let+ common = Common.term
    and+ files =
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE")
    in
    let config = Common.init common in
    let files_to_promote = files_to_promote ~common files in
    Scheduler.go ~common ~config (fun () ->
        Diff_promotion.display files_to_promote)

  let command = Cmd.v info term
end

let info = Cmd.info "promotion"

let group = Cmd.group info [ Run.command; Diff.command ]

let promote = Promote.command
