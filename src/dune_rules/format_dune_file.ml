open Import

module To_file = struct
  module Spec = struct
    type ('path, 'target) t = Dune_lang.Syntax.Version.t * 'path * 'target

    let name = "format-dune-file"
    let version = 1
    let runs_process = false
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

  module A = Action_ext.Make (Spec)
end

module To_stdout = struct
  module Spec = struct
    type ('path, 'target) t = Dune_lang.Syntax.Version.t * 'path

    let name = "format-dune-file-stdout"
    let version = 1
    let runs_process = false
    let bimap (ver, src) f _ = ver, f src
    let is_useful_to ~memoize = memoize

    let encode (version, src) path _target : Sexp.t =
      List [ Dune_lang.Syntax.Version.encode version |> Dune_sexp.to_sexp; path src ]
    ;;

    let action (version, src) ~ectx:_ ~(eenv : Action.env) =
      let stdout = Process.Io.out_channel eenv.stdout_to in
      Dune_lang.Format.format_to_channel ~version ~src stdout;
      Fiber.return ()
    ;;
  end

  module A = Action_ext.Make (Spec)
end

let action ~version (src : Path.t) (dst : Path.Build.t) =
  To_file.A.action (version, src, dst)
;;

let action_stdout ~version (src : Path.t) = To_stdout.A.action (version, src)
