open Import

(* We use the same set of options when producing Wasm code with
   wasm_of_ocaml, since the compilation process is similar and
   generates a JavaScript file with basically the same behavior. *)

module Mode = struct
  type t =
    | JS
    | Wasm

  let equal (a : t) b = Poly.equal a b

  let select ~mode ~js ~wasm =
    match mode with
    | JS -> js
    | Wasm -> wasm
  ;;

  let compare m m' =
    match m, m' with
    | JS, JS -> Eq
    | JS, _ -> Lt
    | _, JS -> Gt
    | Wasm, Wasm -> Eq
  ;;

  let decode =
    let open Dune_sexp.Decoder in
    sum [ "js", return JS; "wasm", return Wasm ]
  ;;

  let to_string mode = select ~mode ~js:"js" ~wasm:"wasm"
  let to_dyn t = Dyn.variant (to_string t) []
  let all = [ JS; Wasm ]

  module Pair = struct
    type 'a t =
      { js : 'a
      ; wasm : 'a
      }

    let select ~mode { js; wasm } = select ~mode ~js ~wasm
    let make v = { js = v; wasm = v }
    let map ~f { js; wasm } = { js = f js; wasm = f wasm }
    let mapi ~f { js; wasm } = { js = f JS js; wasm = f Wasm wasm }

    let map2 ~f { js; wasm } { js = js'; wasm = wasm' } =
      { js = f js js'; wasm = f wasm wasm' }
    ;;
  end

  module Set = struct
    type t = bool Pair.t

    let inter = Pair.map2 ~f:( && )
  end
end

module Sourcemap = struct
  type t =
    | No
    | Inline
    | File

  let decode ~mode =
    let open Decoder in
    match (mode : Mode.t) with
    | JS -> enum [ "no", No; "inline", Inline; "file", File ]
    | Wasm -> enum [ "no", No; "inline", Inline ]
  ;;

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
  let field_oslu name = Ordered_set_lang.Unexpanded.field name

  let decode =
    let open Decoder in
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

  let dump ~mode t =
    let open Dune_engine.Action_builder.O in
    let+ build_runtime = t.build_runtime
    and+ compile = t.compile
    and+ link = t.link in
    let prefix = Mode.to_string mode in
    List.map
      ~f:Encoder.(pair string (list string))
      [ prefix ^ "_of_ocaml_flags", compile
      ; prefix ^ "_of_ocaml_build_runtime_flags", build_runtime
      ; prefix ^ "_of_ocaml_link_flags", link
      ]
  ;;
end

module Compilation_mode = struct
  type t =
    | Whole_program
    | Separate_compilation

  let decode =
    Decoder.enum [ "whole_program", Whole_program; "separate", Separate_compilation ]
  ;;

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
    ; enabled_if : Blang.t option
    ; javascript_files : string list
    ; wasm_files : string list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  let decode ~in_library ~mode =
    let open Decoder in
    let* syntax_version = Syntax.get_exn Stanza.syntax in
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
         ; enabled_if = Some Blang.true_
         ; javascript_files
         ; wasm_files = []
         ; compilation_mode = None
         ; sourcemap = None
         })
    else (
      let only_in_executable decode = if in_library then return None else decode in
      fields
        (let+ flags = Flags.decode
         and+ enabled_if =
           only_in_executable
             (field_o "enabled_if" (Syntax.since Stanza.syntax (3, 17) >>> Blang.decode))
         and+ javascript_files = field "javascript_files" (repeat string) ~default:[]
         and+ wasm_files =
           match (mode : Mode.t) with
           | JS -> return []
           | Wasm ->
             field
               "wasm_files"
               (Syntax.since Stanza.syntax (3, 17) >>> repeat string)
               ~default:[]
         and+ compilation_mode =
           only_in_executable
             (field_o
                "compilation_mode"
                (Syntax.since Stanza.syntax (3, 17) >>> Compilation_mode.decode))
         and+ sourcemap =
           only_in_executable
             (field_o
                "sourcemap"
                (Syntax.since Stanza.syntax (3, 17) >>> Sourcemap.decode ~mode))
         in
         { flags; enabled_if; javascript_files; wasm_files; compilation_mode; sourcemap }))
  ;;

  let default =
    { flags = Flags.standard
    ; enabled_if = None
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
    ; enabled_if : Blang.t option
    ; javascript_files : Path.Build.t list
    ; wasm_files : Path.Build.t list
    ; compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    }

  let make_one ~(dir : Path.Build.t) (x : In_buildable.t) =
    { flags = x.flags
    ; enabled_if = x.enabled_if
    ; javascript_files =
        List.map ~f:(fun name -> Path.Build.relative dir name) x.javascript_files
    ; wasm_files = List.map ~f:(fun name -> Path.Build.relative dir name) x.wasm_files
    ; compilation_mode = x.compilation_mode
    ; sourcemap = x.sourcemap
    }
  ;;

  let make ~dir x = Mode.Pair.map ~f:(fun x -> make_one ~dir x) x

  let default =
    { flags = Flags.standard
    ; enabled_if = None
    ; javascript_files = []
    ; wasm_files = []
    ; compilation_mode = None
    ; sourcemap = None
    }
  ;;

  let force_whole_program_compilation x =
    Mode.Pair.map ~f:(fun x -> { x with compilation_mode = Some Whole_program }) x
  ;;
end

module Ext = struct
  type t = string

  let exe ~mode = Mode.select ~mode ~js:".bc.js" ~wasm:".bc.wasm.js"
  let cmo ~mode = Mode.select ~mode ~js:".cmo.js" ~wasm:".wasmo"
  let cma ~mode = Mode.select ~mode ~js:".cma.js" ~wasm:".wasma"
  let runtime ~mode = Mode.select ~mode ~js:".bc.runtime.js" ~wasm:".bc.runtime.wasma"
  let wasm_dir = ".bc.wasm.assets"
end

module Env = struct
  type 'a t =
    { compilation_mode : Compilation_mode.t option
    ; sourcemap : Sourcemap.t option
    ; runtest_alias : Alias_name.t option
    ; flags : 'a Flags.t
    ; enabled_if : Blang.t option
    }

  let decode ~mode =
    let open Decoder in
    fields
    @@ let+ compilation_mode = field_o "compilation_mode" Compilation_mode.decode
       and+ sourcemap =
         field_o
           "sourcemap"
           (Syntax.since Stanza.syntax (3, 17) >>> Sourcemap.decode ~mode)
       and+ runtest_alias = field_o "runtest_alias" Alias.decode
       and+ flags = Flags.decode
       and+ enabled_if =
         field_o "enabled_if" (Syntax.since Stanza.syntax (3, 17) >>> Blang.decode)
       in
       { compilation_mode; sourcemap; runtest_alias; flags; enabled_if }
  ;;

  let equal { compilation_mode; sourcemap; runtest_alias; flags; enabled_if } t =
    Option.equal Compilation_mode.equal compilation_mode t.compilation_mode
    && Option.equal Sourcemap.equal sourcemap t.sourcemap
    && Option.equal Alias_name.equal runtest_alias t.runtest_alias
    && Flags.equal Ordered_set_lang.Unexpanded.equal flags t.flags
    && Option.equal Blang.equal enabled_if t.enabled_if
  ;;

  let map ~f { compilation_mode; sourcemap; runtest_alias; flags; enabled_if } =
    { compilation_mode; sourcemap; runtest_alias; flags = Flags.map ~f flags; enabled_if }
  ;;

  let empty =
    { compilation_mode = None
    ; sourcemap = None
    ; runtest_alias = None
    ; flags = Flags.standard
    ; enabled_if = None
    }
  ;;

  let default ~profile =
    { compilation_mode = None
    ; sourcemap = None
    ; runtest_alias = None
    ; flags = Flags.default ~profile
    ; enabled_if = None
    }
  ;;
end
