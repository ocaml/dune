open Import
open Dune_lang.Decoder

let name = "ctypes"

let syntax =
  Dune_lang.Syntax.create ~name ~desc:"the ctypes extension"
    [ ((0, 1), `Since (3, 0)); ((0, 2), `Since (3, 4)) ]

module Build_flags_resolver = struct
  module Vendored = struct
    type t =
      { c_flags : Ordered_set_lang.Unexpanded.t
      ; c_library_flags : Ordered_set_lang.Unexpanded.t
      }

    let decode =
      fields
        (let+ c_flags = Ordered_set_lang.Unexpanded.field "c_flags"
         and+ c_library_flags =
           Ordered_set_lang.Unexpanded.field "c_library_flags"
         in
         { c_flags; c_library_flags })
  end

  type t =
    | Pkg_config
    | Vendored of Vendored.t

  let decode =
    let vendored =
      let+ p = Vendored.decode in
      Vendored p
    in
    sum [ ("pkg_config", return Pkg_config); ("vendored", vendored) ]

  let default = Pkg_config
end

module Concurrency_policy = struct
  type t =
    | Sequential
    | Unlocked
    | Lwt_jobs
    | Lwt_preemptive

  let decode =
    enum
      [ ("sequential", Sequential)
      ; ("unlocked", Unlocked)
      ; ("lwt_jobs", Lwt_jobs)
      ; ("lwt_preemptive", Lwt_preemptive)
      ]

  let default = Sequential
end

module Errno_policy = struct
  type t =
    | Ignore_errno
    | Return_errno

  let decode =
    enum [ ("ignore_errno", Ignore_errno); ("return_errno", Return_errno) ]

  let default = Ignore_errno
end

module Headers = struct
  type t =
    | Include of Ordered_set_lang.Unexpanded.t
    | Preamble of String_with_vars.t

  let decode =
    let include_ =
      let+ s = Ordered_set_lang.Unexpanded.decode in
      Include s
    in
    let preamble =
      let+ p = String_with_vars.decode in
      Preamble p
    in
    sum [ ("include", include_); ("preamble", preamble) ]

  let default = Include Ordered_set_lang.Unexpanded.standard
end

module Type_description = struct
  type t =
    { functor_ : Module_name.t
    ; instance : Module_name.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ functor_ = field "functor" Module_name.decode
       and+ instance = field "instance" Module_name.decode in
       { functor_; instance })
end

module Function_description = struct
  type t =
    { concurrency : Concurrency_policy.t
    ; errno_policy : Errno_policy.t
    ; functor_ : Module_name.t
    ; instance : Module_name.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ concurrency = field_o "concurrency" Concurrency_policy.decode
       and+ errno_policy =
         field_o "errno_policy"
           (Dune_lang.Syntax.since syntax (0, 2) >>> Errno_policy.decode)
       and+ functor_ = field "functor" Module_name.decode
       and+ instance = field "instance" Module_name.decode in
       { concurrency =
           Option.value concurrency ~default:Concurrency_policy.default
       ; errno_policy = Option.value errno_policy ~default:Errno_policy.default
       ; functor_
       ; instance
       })
end

type t =
  { external_library_name : External_lib_name.t
  ; build_flags_resolver : Build_flags_resolver.t
  ; headers : Headers.t
  ; type_description : Type_description.t
  ; function_description : Function_description.t list
  ; generated_types : Module_name.t
  ; generated_entry_point : Module_name.t
  ; deps : Dep_conf.t list
  }

type Stanza.t += T of t

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ external_library_name = field "external_library_name" string
     and+ build_flags_resolver =
       field_o "build_flags_resolver" Build_flags_resolver.decode
     and+ type_description = field "type_description" Type_description.decode
     and+ function_description =
       multi_field "function_description" Function_description.decode
     and+ headers = field_o "headers" Headers.decode
     and+ generated_types = field_o "generated_types" Module_name.decode
     and+ generated_entry_point =
       field "generated_entry_point" Module_name.decode
     and+ deps = field_o "deps" (repeat Dep_conf.decode) in
     { external_library_name = External_lib_name.of_string external_library_name
     ; build_flags_resolver =
         Option.value build_flags_resolver ~default:Build_flags_resolver.default
     ; headers = Option.value headers ~default:Headers.default
     ; type_description
     ; function_description
     ; generated_types =
         Option.value generated_types
           ~default:(Module_name.of_string "Types_generated")
     ; generated_entry_point
     ; deps = Option.value ~default:[] deps
     })

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple syntax
    (return [ (name, decode >>| fun x -> [ T x ]) ])

let type_gen_script ctypes =
  sprintf "%s__type_gen"
    (ctypes.external_library_name |> External_lib_name.clean
   |> External_lib_name.to_string)

let module_name_lower_string module_name =
  String.lowercase (Module_name.to_string module_name)

let function_gen_script ctypes (fd : Function_description.t) =
  sprintf "%s__function_gen__%s__%s"
    (ctypes.external_library_name |> External_lib_name.clean
   |> External_lib_name.to_string)
    (module_name_lower_string fd.functor_)
    (module_name_lower_string fd.instance)

let c_generated_types_module ctypes =
  sprintf "%s__c_generated_types"
    (ctypes.external_library_name |> External_lib_name.to_module_name
   |> Module_name.to_string)
  |> Module_name.of_string

let c_generated_functions_module ctypes (fd : Function_description.t) =
  sprintf "%s__c_generated_functions__%s__%s"
    (ctypes.external_library_name |> External_lib_name.clean
   |> External_lib_name.to_string)
    (module_name_lower_string fd.functor_)
    (module_name_lower_string fd.instance)
  |> Module_name.of_string

let c_generated_functions_cout_c ctypes (fd : Function_description.t) =
  sprintf "%s__c_cout_generated_functions__%s__%s.c"
    (External_lib_name.to_string ctypes.external_library_name)
    (module_name_lower_string fd.functor_)
    (module_name_lower_string fd.instance)

let type_gen_script_module ctypes =
  type_gen_script ctypes |> Module_name.of_string

let function_gen_script_module ctypes function_description =
  function_gen_script ctypes function_description |> Module_name.of_string

let generated_modules ctypes =
  List.concat_map ctypes.function_description ~f:(fun function_description ->
      [ function_gen_script_module ctypes function_description
      ; c_generated_functions_module ctypes function_description
      ])
  @ [ type_gen_script_module ctypes
    ; c_generated_types_module ctypes
    ; ctypes.generated_types
    ; ctypes.generated_entry_point
    ]

let non_installable_modules ctypes =
  type_gen_script_module ctypes
  :: List.map ctypes.function_description ~f:(fun function_description ->
         function_gen_script_module ctypes function_description)

let ml_of_module_name mn = Module_name.to_string mn ^ ".ml" |> String.lowercase

let generated_ml_and_c_files ctypes =
  let ml_files = generated_modules ctypes |> List.map ~f:ml_of_module_name in
  let c_files =
    List.map ctypes.function_description ~f:(fun fd ->
        c_generated_functions_cout_c ctypes fd)
  in
  ml_files @ c_files
