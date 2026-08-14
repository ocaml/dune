open Import
open Dune_lang.Decoder

module Name = struct
  include Lib_name.Local

  let equal a b = String.equal (to_string a) (to_string b)
  let hash name = String.hash (to_string name)
  let repr = Repr.view Repr.string ~to_:to_string

  module Map = Map.Make (struct
      type nonrec t = t

      let compare a b = String.compare (to_string a) (to_string b)
      let to_dyn = to_dyn
    end)
end

let syntax =
  Dune_lang.Syntax.create
    ~name:(Dune_lang.Syntax.Name.parse "plugin")
    ~desc:"rule plugin extension"
    ~experimental:true
    [ (0, 1), `Since (3, 26) ]
;;

let () = Dune_project.Extension.register_simple syntax (return [])

let check_enabled =
  let+ (_ : Dune_lang.Syntax.Version.t) = Dune_lang.Syntax.get_exn syntax in
  ()
;;

module Definition = struct
  type t =
    { loc : Loc.t
    ; name : Name.t
    ; executable : string
    }

  let decode =
    let* () = check_enabled in
    let* () = Dune_lang.Syntax.since Stanza.syntax (3, 26) in
    fields
      (let+ loc = loc
       and+ name = field "name" Name.decode
       and+ executable = field "executable" relative_file in
       { loc; name; executable })
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)
end

module Invocation = struct
  type t =
    { loc : Loc.t
    ; name : Name.t
    ; arguments : Dune_lang.Ast.t list
    }

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let of_ast = function
    | Dune_lang.Ast.List (loc, Dune_lang.Ast.Atom (name_loc, A constructor) :: arguments)
      ->
      String.drop_prefix constructor ~prefix:"plugin."
      |> Option.map ~f:(fun name ->
        { loc; name = Name.parse_string_exn (name_loc, name); arguments })
    | _ -> None
  ;;
end
