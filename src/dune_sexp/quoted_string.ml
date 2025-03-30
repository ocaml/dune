open Stdune

type t =
  | Single of string
  | Multi of string list

let to_dyn = function
  | Single s -> Dyn.variant "single" [ Dyn.string s ]
  | Multi l -> Dyn.variant "multi" [ Dyn.list Dyn.string l ]
;;

module Delimiter = struct
  let escaped = "\"\\|"
  let literal = "\"\\>"
end

let to_string = function
  | Single s -> Escape.quoted s
  | Multi s ->
    (* TODO: this is really wrong *)
    List.map ~f:(fun s -> Delimiter.escaped ^ " " ^ Escape.escaped s) s
    |> String.concat ~sep:"\n"
;;

let pp = function
  | Single s -> Pp.verbatim (Escape.quoted s)
  | Multi l ->
    Pp.vbox
      (Pp.concat
         ~sep:Pp.cut
         (List.map l ~f:(fun x ->
            Pp.hbox
              (Pp.concat
                 [ Pp.verbatim Delimiter.escaped
                 ; Pp.space
                 ; Pp.verbatim (Escape.escaped x)
                 ]))
          @ [ Pp.nop ]))
;;

let to_sexp : t -> Sexp.t = function
  | Single s -> List [ Atom "single"; Atom s ]
  | Multi s -> List [ Atom "multi"; List (List.map ~f:(fun x -> Sexp.Atom x) s) ]
;;

let equal a b =
  match a, b with
  | Single a, Single b -> String.equal a b
  | Multi a, Multi b -> List.equal String.equal a b
  | _, _ -> false
;;
