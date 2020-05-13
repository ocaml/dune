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

let run () =
  match Term.eval_choice default all ~catch:true with
  | `Error _ -> exit 1
  | _ -> exit 0

module V1 = struct
  type t = { sanitizer : Sanitizer.t option }

  let make ?default_sanitizer () = { sanitizer = default_sanitizer }

  module Sanitizer = struct
    include Sanitizer

    let default = rewrite_build_path_prefix_map
  end

  let run t =
    let sanitizer =
      match t.sanitizer with
      | Some s -> s
      | None -> Sanitizer.default
    in
    Sanitize.set sanitizer;
    run ()
end
