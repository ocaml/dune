open Import
open Dune_lang.Decoder

(* (js_of_ocaml ...) options are also used when producing Wasm code
   with wasm_of_ocaml, since the compilation process is similar and
   generates a JavaScript file with basically the same behavior. *)

let field_oslu name = Ordered_set_lang.Unexpanded.field name

module Sourcemap = struct
  type t =
    | No
    | Inline
    | File

  let decode = enum [ "no", No; "inline", Inline; "file", File ]

  let equal x y =
    match x, y with
    | No, No -> true
    | Inline, Inline -> true
    | File, File -> true
    | No, _ | Inline, _ | File, _ -> false
  ;;
end

module Flags = struct
  type 'flags t =
    { build_runtime : 'flags
    ; compile : 'flags
    ; link : 'flags
    }

  module Spec = struct
    type nonrec t = Ordered_set_lang.Unexpanded.t t
  end

  let build_runtime t = t.build_runtime
  let compile t = t.compile
  let link t = t.link

  let decode =
    let+ build_runtime = field_oslu "build_runtime_flags"
    and+ compile = field_oslu "flags"
    and+ link = field_oslu "link_flags" in
    { build_runtime; compile; link }
  ;;

  let empty = { build_runtime = []; compile = []; link = [] }

  let default ~profile =
    if Profile.is_dev profile
    then { build_runtime = [ "--pretty" ]; compile = [ "--pretty" ]; link = [] }
    else empty
  ;;

  let map ~f { build_runtime; compile; link } =
    { build_runtime = f build_runtime; compile = f compile; link = f link }
  ;;

  let standard =
    { build_runtime = Ordered_set_lang.Unexpanded.standard
    ; compile = Ordered_set_lang.Unexpanded.standard
    ; link = Ordered_set_lang.Unexpanded.standard
    }
  ;;

  let equal eq { build_runtime; compile; link } x =
    eq build_runtime x.build_runtime && eq compile x.compile && eq link x.link
  ;;

  let make ~spec ~default ~eval =
    let module Proj = struct
      type proj = { proj : 'a. 'a t -> 'a }
    end
    in
    let f { Proj.proj } = eval (proj spec) ~standard:(proj default) in
    let build_runtime = f { proj = build_runtime }
    and compile = f { proj = compile }
    and link = f { proj = link } in
    { build_runtime; compile; link }
  ;;

  let dump t =
    let open Action_builder.O in
    let+ build_runtime = t.build_runtime
    and+ compile = t.compile
    and+ link = t.link in
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ "js_of_ocaml_flags", compile
      ; "js_of_ocaml_build_runtime_flags", build_runtime
      ; "js_of_ocaml_link_flags", link
      ]
  ;;
end

module Submode = struct
  type t =
    | JS
    | Wasm

  let equal = Poly.equal

  module Set = struct
    type t =
      { js : bool
      ; wasm : bool
      }

    let empty = { js = false; wasm = false }

    let add t = function
      | JS -> { t with js = true }
      | Wasm -> { t with wasm = true }
    ;;

    let decode =
      map
        (repeat1 (enum [ "js", JS; "wasm", Wasm ]))
        ~f:(List.fold_left ~init:empty ~f:add)
    ;;

    let equal x y = x.js = y.js && x.wasm = y.wasm

    let to_list x =
      let l = if x.wasm then [ Wasm ] else [] in
      if x.js then JS :: l else l
    ;;
  end
end

module Compilation_mode = struct
  type t =
    | Whole_program
    | Separate_compilation

  let decode = enum [ "whole_program", Whole_program; "separate", Separate_compilation ]

  let equal x y =
    match x, y with
    | Separate_compilation, Separate_compilation -> true
    | Whole_program, Whole_program -> true
    | Separate_compilation, _ -> false
    | Whole_program, _ -> false
  ;;
end

module In_buildable = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t Flags.t
    ; submodes : Submode.Set.t option
    ; javascript_files : string list
    ; wasm_files : string list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  let decode ~executable =
    let* syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    if syntax_version < (3, 0)
    then
      fields
        (let+ flags = Ordered_set_lang.Unexpanded.field "flags"
         and+ javascript_files = field "javascript_files" (repeat string) ~default:[] in
         { flags =
             { build_runtime = Ordered_set_lang.Unexpanded.standard
             ; compile = flags
             ; link = flags (* we set link as well to preserve the old semantic *)
             }
         ; submodes = None
         ; javascript_files
         ; wasm_files = []
         ; compilation_mode = None
         ; sourcemap = None
         })
    else
      fields
        (let+ flags = Flags.decode
         and+ submodes =
           field_o
             "submodes"
             (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> Submode.Set.decode)
         and+ javascript_files = field "javascript_files" (repeat string) ~default:[]
         and+ wasm_files =
           field
             "wasm_files"
             (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> repeat string)
             ~default:[]
         and+ compilation_mode =
           if executable
           then
             field_o
               "compilation_mode"
               (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> Compilation_mode.decode)
           else return None
         and+ sourcemap =
           if executable
           then
             field_o
               "sourcemap"
               (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> Sourcemap.decode)
           else return None
         in
         { flags; submodes; javascript_files; wasm_files; compilation_mode; sourcemap })
  ;;

  let default =
    { flags = Flags.standard
    ; submodes = None
    ; javascript_files = []
    ; wasm_files = []
    ; compilation_mode = None
    ; sourcemap = None
    }
  ;;
end

module In_context = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t Flags.t
    ; submodes : Submode.Set.t option
    ; javascript_files : Path.Build.t list
    ; wasm_files : Path.Build.t list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  let make ~(dir : Path.Build.t) (x : In_buildable.t) =
    { flags = x.flags
    ; submodes = x.submodes
    ; javascript_files =
        List.map ~f:(fun name -> Path.Build.relative dir name) x.javascript_files
    ; wasm_files = List.map ~f:(fun name -> Path.Build.relative dir name) x.wasm_files
    ; compilation_mode = x.compilation_mode
    ; sourcemap = x.sourcemap
    }
  ;;

  let default =
    { flags = Flags.standard
    ; submodes = None
    ; javascript_files = []
    ; wasm_files = []
    ; compilation_mode = None
    ; sourcemap = None
    }
  ;;
end

module Ext = struct
  type t = string

  let select ~submode js wasm =
    match submode with
    | Submode.JS -> js
    | Wasm -> wasm
  ;;

  let exe ~submode = select ~submode ".bc.js" ".bc.wasm.js"
  let cmo ~submode = select ~submode ".cmo.js" ".wasmo"
  let cma ~submode = select ~submode ".cma.js" ".wasma"
  let runtime ~submode = select ~submode ".bc.runtime.js" ".bc.runtime.wasma"
  let wasm_dir = ".bc.wasm.assets"
end

module Env = struct
  type 'a t =
    { compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    ; runtest_alias : Alias.Name.t option
    ; flags : 'a Flags.t
    ; submodes : Submode.Set.t option
    }

  let decode =
    fields
    @@ let+ compilation_mode = field_o "compilation_mode" Compilation_mode.decode
       and+ sourcemap =
         field_o
           "sourcemap"
           (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> Sourcemap.decode)
       and+ runtest_alias = field_o "runtest_alias" Dune_lang.Alias.decode
       and+ flags = Flags.decode
       and+ submodes =
         field_o
           "submodes"
           (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> Submode.Set.decode)
       in
       Option.iter ~f:Alias.register_as_standard runtest_alias;
       { compilation_mode; sourcemap; runtest_alias; flags; submodes }
  ;;

  let equal { compilation_mode; sourcemap; submodes; runtest_alias; flags } t =
    Option.equal Compilation_mode.equal compilation_mode t.compilation_mode
    && Option.equal Sourcemap.equal sourcemap t.sourcemap
    && Option.equal Alias.Name.equal runtest_alias t.runtest_alias
    && Flags.equal Ordered_set_lang.Unexpanded.equal flags t.flags
    && Option.equal Submode.Set.equal submodes t.submodes
  ;;

  let map ~f { compilation_mode; sourcemap; runtest_alias; flags; submodes } =
    { compilation_mode; sourcemap; runtest_alias; flags = Flags.map ~f flags; submodes }
  ;;

  let empty =
    { compilation_mode = None
    ; sourcemap = None
    ; runtest_alias = None
    ; flags = Flags.standard
    ; submodes = None
    }
  ;;

  let default ~profile =
    { compilation_mode = None
    ; sourcemap = None
    ; runtest_alias = None
    ; flags = Flags.default ~profile
    ; submodes = None
    }
  ;;
end
