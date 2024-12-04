open Import

let action =
  let module Spec = struct
    type ('path, 'target) t = Dune_lang.Syntax.Version.t * 'path * 'target

    let name = "format-dune-file"
    let version = 1
    let bimap (ver, src, dst) f g = ver, f src, g dst
    let is_useful_to ~memoize = memoize

    let encode (version, src, dst) path target : Sexp.t =
      List
        [ Dune_lang.Syntax.Version.encode version |> Dune_sexp.to_sexp
        ; path src
        ; target dst
        ]
    ;;

    let action (version, src, dst) ~ectx:_ ~eenv:_ =
      Dune_lang.Format.format_action ~version ~src ~dst;
      Fiber.return ()
    ;;
  end
  in
  let module A = Action_ext.Make (Spec) in
  fun ~version (src : Path.t) (dst : Path.Build.t) -> A.action (version, src, dst)
;;
