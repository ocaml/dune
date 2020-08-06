open! Stdune
open Import
open Dune_lang.Decoder

let is_cram_suffix = String.is_suffix ~suffix:".t"

type test =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

let dyn_of_test =
  let open Dyn.Encoder in
  function
  | File f -> constr "File" [ Path.Source.to_dyn f ]
  | Dir { file; dir } ->
    constr "Dir"
      [ record
          [ ("file", Path.Source.to_dyn file); ("dir", Path.Source.to_dyn dir) ]
      ]

let name t =
  String.drop_suffix ~suffix:".t"
    ( match t with
    | File file -> Path.Source.basename file
    | Dir { file = _; dir } -> Path.Source.basename dir )
  |> Option.value_exn

let script t =
  match t with
  | File f -> f
  | Dir d -> d.file

module Stanza = struct
  type applies_to =
    | Whole_subtree
    | Files_matching_in_this_dir of Predicate_lang.Glob.t

  let default_applies_to = Files_matching_in_this_dir Predicate_lang.Glob.true_

  let decode_applies_to =
    let open Dune_lang.Decoder in
    let subtree =
      let+ _ = keyword ":whole_subtree" in
      Whole_subtree
    in
    let predicate =
      let+ predicate = Predicate_lang.Glob.decode in
      Files_matching_in_this_dir predicate
    in
    subtree <|> predicate

  type t =
    { loc : Loc.t
    ; applies_to : applies_to
    ; alias : Alias.Name.t option
    ; deps : Dep_conf.t Bindings.t option
    ; enabled_if : Blang.t
    }

  let decode =
    fields
      (let+ loc = loc
       and+ applies_to =
         field "applies_to" decode_applies_to ~default:default_applies_to
       and+ alias = field_o "alias" Alias.Name.decode
       and+ deps = field_o "deps" (Bindings.decode Dep_conf.decode)
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None () in
       { loc; alias; deps; enabled_if; applies_to })
end
