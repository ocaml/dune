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
    ; source : string
    }

  let name { name; _ } = name

  let to_dyn { name; source } =
    let open Dyn in
    variant "repository" [ Name.to_dyn name; string source ]
  ;;

  let equal { name; source } t = Name.equal name t.name && String.equal source t.source
  let hash { name; source } = Tuple.T2.hash Name.hash String.hash (name, source)

  let default =
    { name = "default"; source = "git+https://github.com/ocaml/opam-repository.git" }
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ name = field "name" Name.decode
       and+ source = field "source" string in
       { name; source })
  ;;

  let create ~name ~source = { name; source }
  let opam_url { source; _ } = OpamUrl.of_string source
end
