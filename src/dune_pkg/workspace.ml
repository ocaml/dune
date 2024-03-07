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
    ; source : Loc.t * OpamUrl.t
    }

  let name { name; _ } = name

  let to_dyn { name; source = _, source } =
    let open Dyn in
    variant "repository" [ Name.to_dyn name; string (OpamUrl.to_string source) ]
  ;;

  let equal { name; source } t =
    Name.equal name t.name && Tuple.T2.equal Loc.equal OpamUrl.equal source t.source
  ;;

  let hash { name; source } = Tuple.T2.hash Name.hash Poly.hash (name, source)

  let upstream =
    { name = "upstream"
    ; source =
        Loc.none, OpamUrl.of_string "git+https://github.com/ocaml/opam-repository.git"
    }
  ;;

  let overlay =
    { name = "overlay"
    ; source =
        Loc.none, OpamUrl.of_string "git+https://github.com/ocaml-dune/opam-overlays.git"
    }
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ name = field "name" Name.decode
       and+ source = field "source" OpamUrl.decode_loc in
       { name; source })
  ;;

  let opam_url { source; _ } = source
end
