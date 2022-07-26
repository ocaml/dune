open Import

let line_directive ~filename:fn ~line_number =
  let directive =
    if Foreign_language.has_foreign_extension ~fn then "line" else ""
  in
  sprintf "#%s %d %S\n" directive line_number fn

module Spec = struct
  type ('path, 'target) t = 'path * 'target

  let name = "copy-line-directive"

  let version = 1

  let bimap (src, dst) f g = (f src, g dst)

  let is_useful_to ~distribute:_ ~memoize = memoize

  let encode (src, dst) path target : Dune_lang.t =
    List
      [ Dune_lang.atom_or_quoted_string "copy-line-directive"
      ; path src
      ; target dst
      ]

  let action (src, dst) ~ectx:_ ~eenv:_ =
    Io.with_file_in src ~f:(fun ic ->
        Path.build dst
        |> Io.with_file_out ~f:(fun oc ->
               let fn = Path.drop_optional_build_context_maybe_sandboxed src in
               output_string oc
                 (line_directive ~filename:(Path.to_string fn) ~line_number:1);
               Io.copy_channels ic oc));
    Fiber.return ()
end

let action src dst =
  let module M = struct
    type path = Path.t

    type target = Path.Build.t

    module Spec = Spec

    let v = (src, dst)
  end in
  Action.Extension (module M)

let builder ~src ~dst =
  let open Action_builder.O in
  Action_builder.with_file_targets ~file_targets:[ dst ]
    (Action_builder.path src
    >>> Action_builder.return (Action.Full.make (action src dst)))
