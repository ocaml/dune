open Import
open Memo.O

type t =
  | No_implements
  | Virtual of Vimpl.t
  | Parameter of
      { main_module : Module_name.t
      ; implements_parameter : Module_name.t option Resolve.Memo.t
      }

let no_implements = No_implements

let setup_copy_rules_for_impl ~sctx ~dir t =
  match t with
  | No_implements | Parameter _ -> Memo.return ()
  | Virtual vimpl -> Vimpl.setup_copy_rules ~sctx ~dir vimpl
;;

let stubs_o_files = function
  | No_implements | Parameter _ -> []
  | Virtual vimpl -> Vimpl.vlib_stubs_o_files vimpl
;;

let implements_parameter t m =
  match t with
  | Parameter { main_module; implements_parameter }
    when Module_name.equal main_module (Module.name m) -> implements_parameter
  | _ -> Resolve.Memo.return None
;;

let impl_modules t m =
  match t with
  | No_implements | Parameter _ -> Modules.With_vlib.modules m
  | Virtual vimpl -> Modules.With_vlib.impl ~vlib:(Vimpl.vlib_modules vimpl) m
;;

let vimpl_exn = function
  | Virtual vimpl -> vimpl
  | No_implements | Parameter _ -> Code_error.raise "Virtual_rules.vimpl_exn" []
;;

let impl sctx ~(lib : Library.t) ~scope ~for_ =
  match lib.implements with
  | None -> Memo.return No_implements
  | Some (loc, implements) ->
    Lib.DB.find (Scope.libs scope) implements
    >>= (function
     | None ->
       User_error.raise
         ~loc
         [ Pp.textf
             "Cannot implement %s as that library isn't available"
             (Lib_name.to_string implements)
         ]
     | Some vlib ->
       let info = Lib.info vlib in
       (match Lib_info.kind info with
        | Dune_file _ ->
          User_error.raise
            ~loc:lib.buildable.loc
            [ Pp.textf
                "Library %s isn't virtual and cannot be implemented"
                (Lib_name.to_string implements)
            ]
        | Parameter ->
          let main_module = Module_name.of_local_lib_name lib.name in
          Memo.return
            (Parameter { main_module; implements_parameter = Lib.main_module_name vlib })
        | Virtual ->
          let+ vimpl = Vimpl.make ~sctx ~scope ~lib ~info ~vlib ~for_ in
          Virtual vimpl))
;;
