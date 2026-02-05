open Import

let is_valid_global_variable_name = function
  | "root" -> false
  | _ -> true
;;

(* CR-rgrinberg: we need this validation in substitution actions as well *)
let is_valid_package_variable_name = function
  | "hash" | "misc" | "opam-version" | "depends" | "build" | "opamfile" -> false
  | _ -> true
;;

let invalid_variable_error ~loc variable =
  User_error.make
    ~loc
    [ Pp.textf "Variable %S is not supported." (OpamVariable.to_string variable) ]
;;

let opam_variable_to_slang ~loc packages variable =
  let variable_string = OpamVariable.to_string variable in
  let convert_with_package_name package_name =
    match is_valid_package_variable_name variable_string with
    | false -> Error (invalid_variable_error ~loc variable)
    | true ->
      let pform =
        let name = Package_variable_name.of_string variable_string in
        let scope : Package_variable.Scope.t =
          match package_name with
          | None -> Self
          | Some p -> Package (Package_name.of_opam_package_name p)
        in
        Package_variable.to_pform { Package_variable.name; scope }
      in
      Ok (Slang.pform pform)
  in
  match packages with
  | [] ->
    (match is_valid_global_variable_name variable_string with
     | false ->
       (* Note that there's no syntactic distinction between global variables
          and package variables in the current package. This check will prevent
          invalid global variable names from being used for package variables in the
          current package where the optional qualifier "_:" is omitted. *)
       Error (invalid_variable_error ~loc variable)
     | true ->
       (match Pform.Var.of_opam_global_variable_name variable_string with
        | Some global_var -> Ok (Slang.pform (Pform.Var global_var))
        | None -> convert_with_package_name None))
  | [ package_name ] -> convert_with_package_name package_name
  | many ->
    let open Result.O in
    let+ many = Result.List.map many ~f:convert_with_package_name in
    Slang.blang (Blang.And (List.map many ~f:(fun slang -> Blang.Expr slang)))
;;

(* Handles the special case for packages whose names contain '+' characters
   where a special form of string interpolation is used. From the opam manual:
   Warning: if the package name contains a + character (e.g. conf-g++), their
   variables may only be accessed using opam 2.2 via string interpolation,
   with the following syntax:

     "%{?conf-g++:your-variable:}%"
*)
let desugar_special_string_interpolation_syntax
      ((packages, variable, string_converter) as fident)
  =
  match string_converter with
  | Some (package_and_variable, "")
    when List.is_empty packages && String.is_empty (OpamVariable.to_string variable) ->
    (match String.lsplit2 package_and_variable ~on:':' with
     | Some (package, variable) ->
       ( [ Some (OpamPackage.Name.of_string package) ]
       , OpamVariable.of_string variable
       , None )
     | None -> fident)
  | _ -> fident
;;

let opam_fident_to_slang ~loc fident =
  let open Result.O in
  let packages, variable, string_converter =
    OpamFilter.desugar_fident fident |> desugar_special_string_interpolation_syntax
  in
  let+ slang = opam_variable_to_slang ~loc packages variable in
  match string_converter with
  | None -> slang
  | Some (then_, else_) ->
    (* The "else" case is also used when evaluating the condition would expand
       an undefined variable. The catch_undefined_var operator is used to
       convert expressions that throw undefined variable exceptions into false.
    *)
    let condition =
      Blang.Expr (Slang.catch_undefined_var slang ~fallback:(Slang.bool false))
    in
    Slang.if_ condition ~then_:(Slang.text then_) ~else_:(Slang.text else_)
;;

let opam_raw_fident_to_slang ~loc raw_ident =
  OpamTypesBase.filter_ident_of_string raw_ident |> opam_fident_to_slang ~loc
;;

let opam_string_to_slang ~package ~loc opam_string =
  Re.Seq.split_full OpamFilter.string_interp_regex opam_string
  |> Seq.map ~f:(function
    | `Text text -> Ok (Slang.text text)
    | `Delim group ->
      (match Re.Group.get group 0 with
       | "%%" -> Ok (Slang.text "%")
       | interp
         when String.starts_with ~prefix:"%{" interp
              && String.ends_with ~suffix:"}%" interp ->
         let ident = String.sub ~pos:2 ~len:(String.length interp - 4) interp in
         opam_raw_fident_to_slang ~loc ident
       | other ->
         Error
           (User_error.make
              ~loc
              [ Pp.textf
                  "Encountered malformed variable interpolation while processing \
                   commands for package %s."
                  (OpamPackage.to_string package)
              ; Pp.text "The variable interpolation:"
              ; Pp.text other
              ])))
  |> List.of_seq
  |> Result.List.all
  |> Result.map ~f:Slang.concat
;;

let simplify get_solver_var =
  OpamFilter.partial_eval (fun var ->
    match OpamVariable.Full.scope var with
    | Global ->
      let name = OpamVariable.Full.variable var |> Package_variable_name.of_opam in
      if Package_variable_name.equal name Package_variable_name.with_test
      then
        (* We don't generate lockfiles for local packages, and we don't include
           test dependencies for non-local packages, so "with-test" always
           evaluates to "false". *)
        Some (B false)
      else get_solver_var name |> Option.map ~f:Variable_value.to_opam_variable_contents
    | _ -> None)
;;

let partial_eval = function
  | None -> `Filter None
  | Some f ->
    let env = Fun.const None in
    (match OpamFilter.eval_to_bool env f with
     | exception Failure _ -> `Filter (Some f)
     | b -> if b then `Filter None else `Skip)
;;

(* Translate an Opam filter into Dune's "Slang" DSL. The main difference between
   the two languages is in their treatment of undefined package variables. In
   Opam filters, undefined variables take on the value <undefined> which
   is "falsey" in some contexts and propagates through boolean operators if
   their result could be affected by the <undefined> term. Slang doesn't have an
   <undefined> value but raises an exception when an undefined variable is
   expanded. There are two operators in Slang for handling exceptions:

   - "(has_undefined_var <arg>)" evaluates <arg>, discarding the result, and
     returns a boolean which is true iff evaluating <arg> failed due to an
     undefined variable
   - "(catch_undefined_var <value> <fallback>)" evaluates <value> and returns
     the result unless evaluation failed due to an undefined variable, in which
     case the result of <fallback> is returned

   These two Slang operators are used to emulate Opam's undefined value
   semantics.
*)
let to_blang ~package ~loc filter =
  let filter_to_slang (filter : OpamTypes.filter) =
    match filter with
    | FString s -> opam_string_to_slang ~package ~loc s
    | FIdent fident -> opam_fident_to_slang ~loc fident
    | other ->
      Code_error.raise
        "The opam file parser should only allow identifiers and strings in places where \
         strings are expected"
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "full filter", Dyn.string (OpamFilter.to_string filter)
        ; "non-string filter", Dyn.string (OpamFilter.to_string other)
        ]
  in
  let rec filter_to_blang (filter : OpamTypes.filter) =
    let open Result.O in
    match filter with
    | FBool true -> Ok Blang.Ast.true_
    | FBool false -> Ok Blang.Ast.false_
    | (FString _ | FIdent _) as slangable ->
      let+ slang = filter_to_slang slangable in
      Blang.Expr slang
    | FOp (lhs, op, rhs) ->
      let op = Relop.of_opam op in
      let+ lhs = filter_to_slang lhs
      and+ rhs = filter_to_slang rhs in
      Blang.Compare (op, lhs, rhs)
    | FAnd (lhs, rhs) ->
      let+ lhs = filter_to_blang lhs
      and+ rhs = filter_to_blang rhs in
      Blang.Expr (Slang.and_absorb_undefined_var [ lhs; rhs ])
    | FOr (lhs, rhs) ->
      let+ lhs = filter_to_blang lhs
      and+ rhs = filter_to_blang rhs in
      Blang.Expr (Slang.or_absorb_undefined_var [ lhs; rhs ])
    | FNot f ->
      let+ blang = filter_to_blang f in
      Blang.Not blang
    | FDefined f ->
      let+ blang = filter_to_blang f in
      Blang.Not (Blang.Expr (Slang.has_undefined_var (Slang.blang blang)))
    | FUndef _ ->
      Code_error.raise
        "Encountered undefined filter which should not be possible since no filter \
         reduction has taken place."
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ]
  in
  filter_to_blang filter
;;
