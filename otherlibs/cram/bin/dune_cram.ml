open! Stdune
open Cmdliner

let all = [ Run.command; Sanitize.command ]

let default =
  let doc = "Cram test tool" in
  let term = Term.ret (Term.const (`Help (`Pager, None))) in
  ( term
  , Term.info "dune-cram" ~doc
      ~version:
        ( match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v )
      ~man:[ `S "DESCRIPTION"; `P {|Tool simiar to https://bitheap.org/cram/|} ]
  )

let () =
  match Term.eval_choice default all ~catch:true with
  | `Error _ -> exit 1
  | _ -> exit 0
