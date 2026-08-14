open Import
open Dune_lang.Decoder

module Name = struct
  include String

  include Stringlike.Make (struct
      type t = string

      let module_ = "Rule_plugin.Name"
      let description = "rule plugin name"
      let description_of_valid_string = None
      let hint_valid = None
      let of_string_opt name = Some name
      let to_string name = name
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
    | Dune_lang.Ast.List (loc, Dune_lang.Ast.Atom (_, A constructor) :: arguments) ->
      (match String.drop_prefix constructor ~prefix:"plugin." with
       | Some name when not (String.is_empty name) ->
         Some { loc; name = Name.of_string name; arguments }
       | None | Some _ -> None)
    | _ -> None
  ;;
end
