open Import

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; packages : Ordered_set_lang.t
  }

let decode ~dir =
  let open Decoder in
  let+ loc, (directory, packages) =
    fields
      (let+ _, directory = field "dir" dir
       and+ packages = Ordered_set_lang.field "packages" in
       directory, packages)
    |> located
  in
  { loc; directory; packages }
;;

let directory t = t.directory
let loc t = t.loc

let eval_packages t ~standard =
  let standard_set = Package_name.Set.of_list standard in
  let allowed =
    Ordered_set_lang.eval
      t.packages
      ~parse:(fun ~loc:_ s -> Package_name.of_string s)
      ~eq:Package_name.equal
      ~standard
    |> Package_name.Set.of_list
  in
  let invalid = Package_name.Set.diff allowed standard_set in
  if not (Package_name.Set.is_empty invalid)
  then
    User_error.raise
      ~loc:t.loc
      [ Pp.textf
          "The following packages are not available in directory %S: %s"
          t.directory
          (Package_name.Set.to_list invalid
           |> List.map ~f:Package_name.to_string
           |> String.concat ~sep:", ")
      ];
  allowed
;;
