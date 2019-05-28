open! Stdune

type t = {
  none : bool;
  symlink : bool;
  copy : bool;
}

let of_function (f : Sandbox_mode.t -> _) = {
  none = f None;
  symlink = f (Some Symlink);
  copy = f (Some Copy);
}

let no_special_requirements = of_function (fun _ -> true)

let no_sandboxing =
  of_function Option.is_none

let needs_sandboxing =
  of_function Option.is_some

let default = no_special_requirements

let user_rule = no_sandboxing
