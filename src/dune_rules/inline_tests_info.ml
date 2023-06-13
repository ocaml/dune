open Import

module Backend = struct
  let name = Sub_system_name.of_string "inline_tests.backend"

  type t =
    { loc : Loc.t
    ; runner_libraries : (Loc.t * Lib_name.t) list
    ; flags : Ordered_set_lang.Unexpanded.t
    ; list_partitions_flags : Ordered_set_lang.Unexpanded.t option
    ; generate_runner : (Loc.t * Action_unexpanded.t) option
    ; extends : (Loc.t * Lib_name.t) list
    }

  type Sub_system_info.t += T of t

  let loc t = t.loc

  (* The syntax of the driver sub-system is part of the main dune syntax, so we
     simply don't create a new one.

     If we wanted to make the ppx system an extension, then we would create a
     new one. *)
  let syntax = Stanza.syntax

  open Dune_lang.Decoder

  let decode =
    fields
      (let+ loc = loc
       and+ runner_libraries =
         field "runner_libraries" (repeat (located Lib_name.decode)) ~default:[]
       and+ flags = Ordered_set_lang.Unexpanded.field "flags"
       and+ list_partitions_flags =
         field_o "list_partitions_flags"
           (Dune_lang.Syntax.since Stanza.syntax (3, 8)
           >>> Ordered_set_lang.Unexpanded.decode)
       and+ generate_runner =
         field_o "generate_runner" (located Dune_lang.Action.decode_dune_file)
       and+ extends =
         field "extends" (repeat (located Lib_name.decode)) ~default:[]
       in
       { loc
       ; runner_libraries
       ; flags
       ; list_partitions_flags
       ; generate_runner
       ; extends
       })

  let encode t =
    let open Dune_lang.Encoder in
    let lib (_loc, x) = Lib_name.encode x in
    ( (1, 0)
    , record_fields
      @@ [ field_l "runner_libraries" lib t.runner_libraries
         ; field_i "flags" Ordered_set_lang.Unexpanded.encode t.flags
         ; field_i "list_partitions_flags"
             (function
               | None -> []
               | Some x -> Ordered_set_lang.Unexpanded.encode x)
             t.list_partitions_flags
         ; field_o "generate_runner" Dune_lang.Action.encode
             (Option.map t.generate_runner ~f:snd)
         ; field_l "extends" lib t.extends
         ] )
end

module Mode_conf = struct
  module T = struct
    type t =
      | Byte
      | Javascript
      | Native
      | Best

    let compare x y =
      match (x, y) with
      | Byte, Byte -> Eq
      | Byte, _ -> Lt
      | _, Byte -> Gt
      | Javascript, Javascript -> Eq
      | Javascript, _ -> Lt
      | _, Javascript -> Gt
      | Native, Native -> Eq
      | Native, _ -> Lt
      | _, Native -> Gt
      | Best, Best -> Eq

    let to_dyn = Dyn.opaque
  end

  include T
  open Dune_lang.Decoder

  let to_string = function
    | Byte -> "byte"
    | Javascript -> "js"
    | Native -> "native"
    | Best -> "best"

  let decode =
    enum
      [ ("byte", Byte); ("js", Javascript); ("native", Native); ("best", Best) ]

  module O = Comparable.Make (T)
  module Map = O.Map

  module Set = struct
    include O.Set

    let decode = repeat decode >>| of_list

    let default = of_list [ Best ]
  end
end

module Tests = struct
  let name = Sub_system_name.of_string "inline_tests"

  type t =
    { loc : Loc.t
    ; deps : Dep_conf.t list
    ; modes : Mode_conf.Set.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; node_flags : Ordered_set_lang.Unexpanded.t
    ; executable_ocaml_flags : Ocaml_flags.Spec.t
    ; executable_link_flags : Ordered_set_lang.Unexpanded.t
    ; backend : (Loc.t * Lib_name.t) option
    ; libraries : (Loc.t * Lib_name.t) list
    ; enabled_if : Blang.t
    }

  type Sub_system_info.t += T of t

  let loc t = t.loc

  let backends t = Option.map t.backend ~f:(fun x -> [ x ])

  let syntax = Stanza.syntax

  open Dune_lang.Decoder

  let decode =
    fields
      (let+ loc = loc
       and+ deps = field "deps" (repeat Dep_conf.decode) ~default:[]
       and+ flags = Ordered_set_lang.Unexpanded.field "flags"
       and+ node_flags = Ordered_set_lang.Unexpanded.field "node_flags"
       and+ executable_ocaml_flags, executable_link_flags =
         field "executable"
           ~default:
             (Ocaml_flags.Spec.standard, Ordered_set_lang.Unexpanded.standard)
           (Dune_lang.Syntax.since Stanza.syntax (2, 8)
           >>> fields
                 (let+ ocaml_flags = Ocaml_flags.Spec.decode
                  and+ link_flags =
                    Ordered_set_lang.Unexpanded.field "link_flags"
                      ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
                  in
                  (ocaml_flags, link_flags)))
       and+ backend = field_o "backend" (located Lib_name.decode)
       and+ libraries =
         field "libraries" (repeat (located Lib_name.decode)) ~default:[]
       and+ modes =
         field "modes"
           (Dune_lang.Syntax.since syntax (1, 11) >>> Mode_conf.Set.decode)
           ~default:Mode_conf.Set.default
       and+ enabled_if =
         Enabled_if.decode ~allowed_vars:Any ~is_error:true
           ~since:(Some (3, 0))
           ()
       in
       { loc
       ; deps
       ; flags
       ; node_flags
       ; executable_ocaml_flags
       ; executable_link_flags
       ; backend
       ; libraries
       ; modes
       ; enabled_if
       })

  (* We don't use this at the moment, but we could implement it for debugging
     purposes *)
  let encode _t = assert false
end
