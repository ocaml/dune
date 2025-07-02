open! Stdune

type t =
  { packages : Package_dependency.t list
  ; url : string option
  }

let encode { packages; url } =
  let open Dune_sexp.Encoder in
  match packages, url with
  | [], Some url -> string url
  | _ ->
    list sexp
    @@ record_fields
         [ field_l "depends" Package_dependency.encode packages
         ; field_o "url" string url
         ]
;;

let decode ~toplevel =
  let open Dune_sexp.Decoder in
  let+ res =
    either string
    @@ fields
         (let+ loc, packages =
            located @@ field ~default:[] "depends" (repeat Package_dependency.decode)
          and+ url = field_o "url" string in
          let () =
            if toplevel
            then
              User_warning.emit
                ~loc
                [ Pp.textf
                    "The depends field of the documentation stanza can only be non-empty \
                     when the documentation stanza is inside a package stanza."
                ]
          in
          { packages; url })
  in
  match res with
  | Left url -> { packages = []; url = Some url }
  | Right res -> res
;;

let to_dyn { packages; url } =
  let open Dyn in
  record [ "url", option string url; "packages", list Package_dependency.to_dyn packages ]
;;

let superpose d1 d2 =
  let url =
    match d2.url with
    | Some _ as u -> u
    | None -> d1.url
  in
  { url; packages = d2.packages }
;;
