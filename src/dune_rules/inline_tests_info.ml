open! Dune_engine
open! Stdune
open Import

module Backend = struct
  let name = Sub_system_name.make "inline_tests.backend"

  type t =
    { loc : Loc.t
    ; runner_libraries : (Loc.t * Lib_name.t) list
    ; flags : Ordered_set_lang.Unexpanded.t
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
       and+ generate_runner =
         field_o "generate_runner" (located Action_dune_lang.decode)
       and+ extends =
         field "extends" (repeat (located Lib_name.decode)) ~default:[]
       in
       { loc; runner_libraries; flags; generate_runner; extends })

  let encode t =
    let open Dune_lang.Encoder in
    let lib (_loc, x) = Lib_name.encode x in
    ( (1, 0)
    , record_fields
      @@ [ field_l "runner_libraries" lib t.runner_libraries
         ; field_i "flags" Ordered_set_lang.Unexpanded.encode t.flags
         ; field_o "generate_runner" Action_dune_lang.encode
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

    let compare (a : t) b = Poly.compare a b

    let to_dyn _ = Dyn.opaque
  end

  include T
  open Dune_lang.Decoder

  let decode =
    enum
      [ ("byte", Byte); ("js", Javascript); ("native", Native); ("best", Best) ]

  module O = Comparable.Make (T)

  module Set = struct
    include O.Set

    let decode = repeat decode >>| of_list

    let default = of_list [ Best ]
  end
end

module Tests = struct
  let name = Sub_system_name.make "inline_tests"

  type t =
    { loc : Loc.t
    ; deps : Dep_conf.t list
    ; modes : Mode_conf.Set.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; executable : Ocaml_flags.Spec.t
    ; backend : (Loc.t * Lib_name.t) option
    ; libraries : (Loc.t * Lib_name.t) list
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
       and+ executable =
         field "executable" ~default:Ocaml_flags.Spec.standard
           (Dune_lang.Syntax.since Stanza.syntax (2, 8)
           >>> fields Ocaml_flags.Spec.decode)
       and+ backend = field_o "backend" (located Lib_name.decode)
       and+ libraries =
         field "libraries" (repeat (located Lib_name.decode)) ~default:[]
       and+ modes =
         field "modes"
           (Dune_lang.Syntax.since syntax (1, 11) >>> Mode_conf.Set.decode)
           ~default:Mode_conf.Set.default
       in
       { loc; deps; flags; executable; backend; libraries; modes })

  (* We don't use this at the moment, but we could implement it for debugging
     purposes *)
  let encode _t = assert false
end
