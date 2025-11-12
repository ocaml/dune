open Import

module Repository = struct
  module Name = struct
    include String

    let pp v = Pp.text v

    include (
      Stringlike.Make (struct
        type nonrec t = t

        let description_of_valid_string = None
        let hint_valid = None
        let to_string t = t
        let module_ = "Repository.Name"
        let description = "Opam Repository name"
        let of_string_opt name = Some name
      end) :
        Stringlike with type t := t)
  end

  type t =
    { name : Name.t
    ; url : Loc.t * OpamUrl.t
    }

  let name { name; _ } = name

  let to_dyn { name; url = _, url } =
    let open Dyn in
    variant "repository" [ Name.to_dyn name; string (OpamUrl.to_string url) ]
  ;;

  let equal { name; url } t =
    Name.equal name t.name && Tuple.T2.equal Loc.equal OpamUrl.equal url t.url
  ;;

  let hash { name; url } = Tuple.T2.hash Name.hash Poly.hash (name, url)

  let upstream =
    { name = "upstream"
    ; url = Loc.none, OpamUrl.of_string "git+https://github.com/ocaml/opam-repository.git"
    }
  ;;

  let overlay =
    { name = "overlay"
    ; url =
        Loc.none, OpamUrl.of_string "git+https://github.com/ocaml-dune/opam-overlays.git"
    }
  ;;

  let binary_packages =
    { name = "binary-packages"
    ; url =
        ( Loc.none
        , OpamUrl.of_string "git+https://github.com/ocaml-dune/ocaml-binary-packages.git"
        )
    }
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ name = field "name" Name.decode
       and+ url = field "url" OpamUrl.decode_loc in
       { name; url })
  ;;

  let opam_url { name = _; url } = url
end

let dev_tool_path_to_source_dir path =
  let of_ =
    Path.Build.relative Path.Build.root ".dev-tools.locks"
    |> Path.build
    |> Path.to_absolute_filename
    |> Path.External.of_string
  in
  let in_dev_tool_dir = Path.External.is_descendant ~of_ path in
  match in_dev_tool_dir with
  | false ->
    Code_error.raise
      "External lock dir path is unsupported"
      [ "dir", Path.External.to_dyn path ]
  | true ->
    let prefix = Path.External.to_string of_ in
    let as_string = Path.External.to_string path in
    let relative =
      String.sub
        as_string
        ~pos:(String.length prefix)
        ~len:(String.length as_string - String.length prefix)
    in
    let exploded = String.split ~on:'/' relative in
    (match exploded with
     | "" :: dev_tool_name :: components ->
       Path.Source.L.relative
         Path.Source.root
         ([ "_build"; ".dev-tools.locks"; dev_tool_name ] @ components)
     | components ->
       Code_error.raise
         "Unexpected external path"
         [ "dir", Path.External.to_dyn path; "components", Dyn.(list string) components ])
;;
