(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative definition of command line interfaces.

    Consult the {{!page-tutorial}tutorial}, the
    {{!page-cookbook}cookbook}, program
    {{!page-cookbook.blueprints}blueprints} and
    {{!page-cookbook.tip_src_structure}source structure}, details about the
    supported {{!page-cli}command line syntax} and
    {{!page-examples}examples} of use.

    Open the module to use it, it defines only these modules in your
    scope. *)

(** Man pages.

    Man page generation is automatically handled by [Cmdliner], see
    the {{!page-tool_man.manual}details}. The {!Manpage.block} type is
    used to define a man page's content. It's a good idea to follow
    the {{!Manpage.standard_sections}standard} manual page structure.

   {b References.}
   {ul
   {- [man-pages(7)], {{:http://man7.org/linux/man-pages/man7/man-pages.7.html}
      {e Conventions for writing Linux man pages}}.}} *)
module Manpage : sig

  (** {1:man Man pages} *)

  type section_name = string
  (** The type for section names (titles). See
      {{!standard_sections}standard section names}. *)

  type block =
  [ `S of section_name (** Start a new section with given name. *)
  | `P of string (** Paragraph with given text. *)
  | `Pre of string (** Preformatted paragraph with given text. *)
  | `I of string * string (** Indented paragraph with given label and text. *)
  | `Noblank (** Suppress blank line introduced between two blocks. *)
  | `Blocks of block list (** Splice given blocks. *) ]
  (** The type for a block of man page text.

      Except in [`Pre], whitespace and newlines are not significant
      and are all collapsed to a single space. All block strings
      support the {{!page-tool_man.doclang}documentation markup language}.*)

  val escape : string -> string
  (** [escape s] escapes [s] so that it doesn't get interpreted by the
      {{!page-tool_man.doclang}documentation markup language}. *)

  type title = string * int * string * string * string
  (** The type for man page titles. Describes the man page
      [title], [section], [center_footer], [left_footer], [center_header]. *)

  type t = title * block list
  (** The type for a man page. A title and the page text as a list of blocks. *)

  type xref =
  [ `Main (** Refer to the man page of the program itself. *)
  | `Cmd of string (** Refer to the command [cmd] of the tool, which must
                       exist. *)
  | `Tool of string (** Tool refer to the given command line tool. *)
  | `Page of string * int (** Refer to the manpage [name(sec)]. *) ]
  (** The type for man page cross-references. *)

  (** {1:standard_sections Standard section names and content}

      The following are standard man page section names, roughly ordered
      in the order they conventionally appear. See also
      {{:http://man7.org/linux/man-pages/man7/man-pages.7.html}[man man-pages]}
      for more elaborations about what sections should contain. *)

  val s_name : section_name
  (** The [NAME] section. This section is automatically created by
      [Cmdliner] for your command. *)

  val s_synopsis : section_name
  (** The [SYNOPSIS] section. By default this section is automatically
      created by [Cmdliner] for your command, unless it is the first
      section of your term's man page, in which case it will replace
      it with yours. *)

  val s_description : section_name
  (** The [DESCRIPTION] section. This should be a description of what
      the tool does and provide a little bit of command line usage and
      documentation guidance. *)

  val s_commands : section_name
  (** The [COMMANDS] section. By default subcommands get listed here. *)

  val s_arguments : section_name
  (** The [ARGUMENTS] section. By default positional arguments get
      listed here. *)

  val s_options : section_name
  (** The [OPTIONS] section. By default optional arguments get
      listed here. *)

  val s_common_options : section_name
  (** The [COMMON OPTIONS] section. By default help and version options get
      listed here. For programs with multiple commands, optional arguments
      common to all commands can be added here. *)

  val s_exit_status : section_name
  (** The [EXIT STATUS] section. By default term status exit codes
      get listed here. *)

  val s_environment : section_name
  (** The [ENVIRONMENT] section. By default environment variables get
      listed here. *)

  val s_environment_intro : block
  (** [s_environment_intro] is the introduction content used by cmdliner
      when it creates the {!s_environment} section. *)

  val s_files : section_name
  (** The [FILES] section. *)

  val s_bugs : section_name
  (** The [BUGS] section. *)

  val s_examples : section_name
  (** The [EXAMPLES] section. *)

  val s_authors : section_name
  (** The [AUTHORS] section. *)

  val s_see_also : section_name
  (** The [SEE ALSO] section. *)

  val s_none : section_name
  (** [s_none] is a special section named ["cmdliner-none"] that can be used
      whenever you do not want something to be listed. *)

  (** {1:output Output}

    The {!print} function can be useful if the client wants to define
    other man pages (e.g. to implement a help command). *)

  type format =
  [ `Auto (** Format like [`Pager] or [`Plain] whenever the [TERM]
              environment variable is [dumb] or unset. *)
  | `Pager (** {{!page-cli.help}Tries} to use a pager or falls back
               to [`Plain]. *)
  | `Plain (** Format to plain text. *)
  | `Groff (** Format to groff commands. *) ]
  (** The type for man page output specification. *)

  val print :
    ?env:(string -> string option) -> ?errs:Format.formatter ->
    ?subst:(string -> string option) -> format -> Format.formatter -> t -> unit
  (** [print ~env ~errs ~subst fmt ppf page] prints [page] on [ppf] in the
      format [fmt].
      {ul
      {- [env] is used to lookup environment for driving paging when the
         format is [`Pager]. Defaults to {!Sys.getenv_opt}.}
      {- [subst] can be used to perform variable
         substitution (defaults to the identity).}
      {- [errs] is used to print formatting errors, it defaults to
         {!Format.err_formatter}.}} *)
end

(** Terms.

    A term made of terms referring to {{!Arg.argterms}command line arguments}
    implicitly defines a command line syntax fragment. Terms are associated
    to command values {!Cmd.t} which are
    {{!Cmd.section-eval}evaluated} to eventually produce an
    {{!Cmd.Exit.code}exit code}.

    Nowadays terms are best defined using the {!Cmdliner.Term.Syntax}.
    See examples in the {{!page-cookbook.blueprints}blueprints}. *)
module Term : sig

  (** {1:terms Terms} *)

  type +'a t
  (** The type for terms evaluating to values of type ['a]. *)

  val const : 'a -> 'a t
  (** [const v] is a term that evaluates to [v]. *)

  val app : ('a -> 'b) t -> 'a t -> 'b t
  (** [app f v] is a term that evaluates to the result applying
      the evaluation of [v] to the one of [f]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] is [app (const f) t]. *)

  val product : 'a t -> 'b t  -> ('a * 'b) t
  (** [product t0 t1] is [app (app (map (fun x y -> (x, y)) t0) t1)] *)

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f $ v] is {!app}[ f v]. *)

  (** [let] operators.

      See how to use them in the {{!page-cookbook.blueprints}blueprints}. *)
  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    (** [( let+ )] is {!map}. *)

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    (** [( and+ )] is {!product}. *)
  end

  (** {1 Interacting with {!Cmd.t} evaluation}

      These special terms allow to interact with the
      {{!Cmd.section-eval_low}low-level evaluation process} performed
      on commands. *)

  val term_result : ?usage:bool -> ('a, [`Msg of string]) result t -> 'a t
  (** [term_result] is such that:
      {ul
      {- [term_result ~usage (Ok v)] {{!Cmd.eval_value}evaluates}
         to [Ok (`Ok v)].}
      {- [term_result ~usage (Error (`Msg e))]
         {{!Cmd.eval_value}evaluates} to [Error `Term] with the error message
         [e] and usage shown according to [usage] (defaults to [false])}}

      See also {!term_result'}. *)

  val term_result' : ?usage:bool -> ('a, string) result t -> 'a t
  (** [term_result'] is like {!term_result} but with a [string]
      error case. *)

  val cli_parse_result : ('a, [`Msg of string]) result t -> 'a t
  (** [cli_parse_result] is such that:
      {ul
      {- [cli_parse_result (Ok v)] {{!Cmd.eval_value}evaluates}
         [Ok (`Ok v)).}
      {- [cli_parse_result (Error (`Msg e))]] {{!Cmd.eval_value}evaluates}
         [Error `Parse].}}
      See also {!cli_parse_result'}. *)

  val cli_parse_result' : ('a, string) result t -> 'a t
  (** [cli_parse_result'] is like {!cli_parse_result} but with a
      [string] error case. *)

  val main_name : string t
  (** [main_name] is a term that evaluates to the main command name;
      that is the name of the tool. *)

  val choice_names : string list t
  (** [choice_names] is a term that evaluates to the names of the commands
      that are children of the main command. *)

  val with_used_args : 'a t -> ('a * string list) t
  (** [with_used_args t] is a term that evaluates to [t] tupled
      with the arguments from the command line that where used to
      evaluate [t]. *)

  type 'a ret =
  [ `Help of Manpage.format * string option
  | `Error of (bool * string)
  | `Ok of 'a ]
  (** The type for command return values. See {!val-ret}. *)

  val ret : 'a ret t -> 'a t
  (** [ret v] is a term whose evaluation depends on the case
      to which [v] evaluates. With :
      {ul
      {- [`Ok v], it evaluates to [v].}
      {- [`Error (usage, e)], the evaluation fails and [Cmdliner] prints
         the error [e] and the term's usage if [usage] is [true].}
      {- [`Help (format, name)], the evaluation fails and [Cmdliner] prints
         a manpage in format [format]. If [name] is [None] this is the
         the main command's manpage. If [name] is [Some c] this is
         the man page of the subcommand [c] of the main command.}} *)

  val env : (string -> string option) t
  (** [env] is the [env] argument given to {{!Cmd.section-eval}command
      evaluation functions}. If you need to refine the environment
      lookup done by Cmdliner's machinery you should use this rather
      than direct calls to {!Sys.getenv_opt}. *)
end

(** Commands.

    Command line syntaxes are implicitely defined by {!Term.t}
    values. A command value binds a term and its documentation to a
    command name.

    A command can group a list of subcommands (and recursively). In this
    case your tool defines a tree of commands, each with its own command
    line syntax. The root of that tree is called the {e main command};
    it represents your tool and its name. *)
module Cmd : sig

  (** {1:info Command information}

      Command information defines the name and documentation of a command. *)

  (** Exit codes and their information. *)
  module Exit : sig

    (** {1:codes Exit codes} *)

    type code = int
    (** The type for exit codes.

        {b Warning.} You should avoid status codes strictly greater than 125
        as those may be used by
        {{:https://www.gnu.org/software/bash/manual/html_node/Exit-Status.html}
        some} shells. *)

    (** {2:predefined Predefined codes}

        These are documented by {!defaults}. *)

    val ok : code
    (** [ok] is [0], the exit status for success. *)

    val some_error : code
    (** [some_error] is [123], an exit status for indiscriminate errors
        reported on [stderr]. *)

    val cli_error : code
    (** [cli_error] is [124], an exit status for command line parsing
        errors. *)

    val internal_error : code
    (** [internal_error] is [125], an exit status for unexpected internal
        errors. *)

    (** {1:info Exit code information} *)

    type info
    (** The type for exit code information. *)

    val info :
      ?docs:Manpage.section_name -> ?doc:string -> ?max:code -> code -> info
    (** [info ~docs ~doc min ~max] describe the range of exit
        statuses from [min] to [max] (defaults to [min]).
        {ul
        {- [doc] is the man page information for the statuses,
           defaults to ["undocumented"]. The
           {{!page-tool_man.doclang}documentation markup language}
           can be used with following variables:
           {ul
           {- [$(status)], the value of [min].}
           {- [$(status_max)], the value of [max].}
           {- The variables mentioned in the documentation of
              {!Cmd.val-info}}}}
        {- [docs] is the title of the man page section in which the statuses
           will be listed, it defaults to {!Manpage.s_exit_status}.}} *)

    val info_code : info -> code
    (** [info_code i] is the minimal code of [i]. *)

    val defaults : info list
    (** [defaults] are exit code information for {!ok}, {!some_error},
        {!cli_error} and {!internal_error}. *)
  end

  (** Environment variable and their information. *)
  module Env : sig

    (** {1:envvars Environment variables} *)

    type var = string
    (** The type for environment variable names. *)

    (** {1:info Environment variable information} *)

    type info
    (** The type for environment variable information. *)

    val info :
      ?deprecated:string -> ?docs:Manpage.section_name -> ?doc:string -> var ->
      info
    (** [info ~docs ~doc var] describes an environment variable
        [var] such that:
        {ul
        {- [doc] is the man page information of the environment
            variable, defaults to ["See option $(opt)."].}
        {- [docs] is the title of the man page section in which the environment
          variable will be listed, it defaults to
          {!Cmdliner.Manpage.s_environment}.}
        {- [deprecated], if specified the environment variable is
           deprecated.  Use of the variable warns on dep[stderr] This
           message which should be a capitalized sentence is
           preprended to [doc] and output on standard error when the
           environment variable ends up being used.}}

        In [doc] and [deprecated] the {{!page-tool_man.doclang}documentation
        markup language} can be used with following variables:

        {ul
        {- [$(opt)], if any the option name of the argument the variable is
           looked up for.}
        {- [$(env)], the value of [var].}
        {- The variables mentioned in the doc string of {!Cmd.val-info}.}} *)

    val info_var : info -> var
    (** [info_var info] is the variable described by [info]. *)
  end

  type info
  (** The type for information about commands. *)

  val info :
    ?deprecated:string -> ?man_xrefs:Manpage.xref list ->
    ?man:Manpage.block list -> ?envs:Env.info list -> ?exits:Exit.info list ->
    ?sdocs:Manpage.section_name -> ?docs:Manpage.section_name -> ?doc:string ->
    ?version:string -> string -> info
  (** [info ?sdocs ?man ?docs ?doc ?version name] is a term information
      such that:
      {ul
      {- [name] is the name of the command.}
      {- [version] is the version string of the command line tool, this
         is only relevant for the main command and ignored otherwise.}
      {- [deprecated], if specified the command is deprecated. Use of the
          variable warns on [stderr]. This
          message which should be a capitalized sentence is
          preprended to [doc] and output on standard error when the
          environment variable ends up being used.}
      {- [doc] is a one line description of the command used
         for the [NAME] section of the command's man page and in command
         group listings.}
      {- [docs], for commands that are part of a group, the title of the
         section of the parent's command man page where it should be listed
         (defaults to {!Manpage.s_commands}).}
      {- [sdocs] defines the title of the section in which the
         standard [--help] and [--version] arguments are listed
         (defaults to {!Manpage.s_common_options}).}
      {- [exits] is a list of exit statuses that the command evaluation
         may produce, defaults to {!Exit.defaults}.}
      {- [envs] is a list of environment variables that influence
         the command's evaluation.}
      {- [man] is the text of the man page for the command.}
      {- [man_xrefs] are cross-references to other manual pages. These
         are used to generate a {!Manpage.s_see_also} section.}}

      [doc], [deprecated], [man], [envs], [exits] support the
      {{!page-tool_man.doclang} documentation markup language} in which the
      following variables are recognized:

      {ul
      {- [$(tool)] the main, topmost, command name.}
      {- [$(cmd)] the command invocation from main command to the
         command name.}
      {- [$(cmd.name)] the command's name.}
      {- [$(cmd.parent)] the command's parent or the main command if none.}}

      Previously some of these names were refered to as [$(tname)],
      [$(mname)] and [$(iname)], they still work but do not use them,
      they are obscure. *)


  (** {1:cmds Commands} *)

  type 'a t
  (** The type for commands whose evaluation result in a value of
      type ['a]. *)

  val make : info -> 'a Term.t -> 'a t
  (** [make i t] is a command with information [i] and command line syntax
      parsed by [t]. *)

  val v : info -> 'a Term.t -> 'a t
  (** [v] is an old name for {!make} which should be preferred. *)

  val group : ?default:'a Term.t -> info -> 'a t list -> 'a t
  (** [group i ?default cmds] is a command with information [i] that
      groups subcommands [cmds]. [default] is the command line syntax
      to parse if no subcommand is specified on the command line. If
      [default] is [None] (default), the tool errors when no subcommand
      is specified. *)

  val name : 'a t -> string
  (** [name c] is the name of [c]. *)

  (** {1:eval Evaluation}

      Read {!page-cookbook.cmds_which_eval} in the cookbook if you
      struggle to choose between this menagerie of evaluation
      functions.

      These functions are meant to be composed with {!Stdlib.exit}.
      The following exit codes may be returned by all these functions:
      {ul
      {- {!Exit.cli_error} if a parse error occurs.}
      {- {!Exit.internal_error} if the [~catch] argument is [true] (default)
         and an uncaught exception is raised.}
      {- The value of [~term_err] (defaults to {!Exit.cli_error}) if
         a term error occurs.}}

      These exit codes are described in {!Exit.defaults} which is the
      default value of the [?exits] argument of the function {!val-info}. *)

  val eval :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array ->
    ?term_err:Exit.code -> unit t -> Exit.code
  (** [eval cmd] is {!Exit.ok} if [cmd] evaluates to [()].
      See {!eval_value} for other arguments. *)

  val eval' :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array ->
    ?term_err:Exit.code -> Exit.code t -> Exit.code
  (** [eval' cmd] is [c] if [cmd] evaluates to the exit code [c].
      See {!eval_value} for other arguments. *)

  val eval_result :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array ->
    ?term_err:Exit.code -> (unit, string) result t -> Exit.code
  (** [eval_result cmd] is:
      {ul
      {- {!Exit.ok} if [cmd] evaluates to [Ok ()].}
      {- {!Exit.some_error} if [cmd] evaluates to [Error msg]. In this
         case [msg] is printed on [err].}}
      See {!eval_value} for other arguments. *)

  val eval_result' :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array ->
    ?term_err:Exit.code -> (Exit.code, string) result t -> Exit.code
  (** [eval_result' cmd] is:
      {ul
      {- [c] if [cmd] evaluates to [Ok c].}
      {- {!Exit.some_error} if [cmd] evaluates to [Error msg]. In this
         case [msg] is printed on [err].}}
      See {!eval_value} for other arguments. *)

  (** {2:eval_low Low level evaluation}

      This interface gives more information on command evaluation results
      and lets you choose how to map evaluation results to exit codes.
      All evaluation functions are wrappers around {!eval_value}. *)

  type 'a eval_ok =
  [ `Ok of 'a (** The term of the command evaluated to this value. *)
  | `Version (** The version of the main cmd was requested. *)
  | `Help (** Help was requested. *) ]
  (** The type for successful evaluation results. *)

  type eval_error =
  [ `Parse (** A parse error occurred. *)
  | `Term (** A term evaluation error occurred. *)
  | `Exn (** An uncaught exception occurred. *) ]
  (** The type for erroring evaluation results. *)

  val eval_value :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array -> 'a t ->
    ('a eval_ok, eval_error) result
  (** [eval ~help ~err ~catch ~env ~argv cmd] is the evaluation result
      of [cmd] with:
      {ul
      {- [argv] the command line arguments to parse (defaults to {!Sys.argv})}
      {- [env] the function used for environment variable lookup (defaults
         to {!Sys.getenv}).}
      {- [catch] if [true] (default) uncaught exceptions
         are intercepted and their stack trace is written to the [err]
         formatter}
      {- [help] is the formatter used to print help, version messages
         or completions, (defaults to {!Format.std_formatter}). Note
         that the completion protocol needs to output ['\n'] line ending,
         if you are outputing to a channel make sure it is in binary
         mode to avoid newline translation (this is done automatically
         before completion when [help] is {!Format.std_formatter}).}
      {- [err] is the formatter used to print error messages
         (defaults to {!Format.err_formatter}).}} *)

  type 'a eval_exit =
  [ `Ok of 'a (** The term of the command evaluated to this value. *)
  | `Exit of Exit.code (** The evaluation wants to exit with this code. *) ]
  (** The type for evaluation exits. *)

  val eval_value' :
    ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
    ?env:(string -> string option) -> ?argv:string array -> ?term_err:int ->
    'a t -> 'a eval_exit
  (** [eval_value'] is like {!eval_value}, but if the command term
      does not evaluate to [Ok (`Ok v)], returns an exit code like the
      higher-level {{!val-eval}evaluation} functions do (which can be
      {!Exit.ok} in case help or version was requested). *)

  val eval_peek_opts :
    ?version_opt:bool -> ?env:(string -> string option) ->
    ?argv:string array -> 'a Term.t ->
    'a option * ('a eval_ok, eval_error) result
  (** {b WARNING.} You are highly encouraged not to use this
      function it may be removed in the future.

      [eval_peek_opts version_opt argv t] evaluates [t], a term made
      of optional arguments only, with the command line [argv]
      (defaults to {!Sys.argv}). In this evaluation, unknown optional
      arguments and positional arguments are ignored.

      The evaluation returns a pair. The first component is
      the result of parsing the command line [argv] stripped from
      any help and version option if [version_opt] is [true] (defaults
      to [false]). It results in:
      {ul
      {- [Some _] if the command line would be parsed correctly given the
         {e partial} knowledge in [t].}
      {- [None] if a parse error would occur on the options of [t]}}

      The second component is the result of parsing the command line
      [argv] without stripping the help and version options. It
      indicates what the evaluation would result in on [argv] given
      the partial knowledge in [t] (for example it would return
      [`Help] if there's a help option in [argv]). However in
      contrasts to {!val-eval_value} no side effects like error
      reporting or help output occurs.

      {b Note.} Positional arguments can't be peeked without the full
      specification of the command line: we can't tell apart a
      positional argument from the value of an unknown optional
      argument. *)
end

(** Terms for command line arguments.

    This module provides functions to define terms that evaluate
    to the arguments provided on the command line.

    Basic constraints, like the argument type or repeatability, are
    specified by defining a value of type {!Arg.t}. Further constraints can
    be specified during the {{!Arg.argterms}conversion} to a term. *)
module Arg : sig

  (** {1:argconv Argument converters} *)

  (** Argument completion.

      This module provides a type to describe how positional and
      optional argument values of {{!Arg.type-conv}argument
      converters} can be completed. It defines which completion
      directives from the {{!page-cli.completion_protocol}protocol}
      get emitted by your tool for the argument.

      {b Note.} Subcommand and option name are completed
      automatically by the library itself and
      {{!Cmdliner.Arg.predef}prefined argument converters} already
      have completions built-in whenever appropriate. *)
  module Completion : sig

    (** {1:directives Completion directives} *)

    type 'a directive
    (** The type for a completion directive for values of type ['a]. *)

    val value : ?doc:string -> 'a -> 'a directive
    (** [value v ~doc] indicates that the token to complete could be
        replaced by the value [v] as serialized by the argument's
        formatter {!Conv.pp}. [doc] is ANSI styled UTF-8 text
        documenting the value, defaults to [""]. *)

    val string : ?doc:string -> string -> 'a directive
    (** [string s ~doc] indicates that the token to complete could be
        replaced by the string [s]. [doc] is ANSI styled UTF-8 text
        documenting the value, defaults to [""]. *)

    val files : 'a directive
    (** [files] indicates that the token to complete could be replaced
        with files that the shell deems suitable. *)

    val dirs : 'a directive
    (** [dirs] indicates that the token to complete could be replaced with
        directories that the shell deems suitable. *)

    val restart : 'a directive
    (** [restart] indicates that the shell should restart the completion
        after the positional disambiguation token [--].

        This is typically used for tools that end-up invoking other
        tools like [sudo -- TOOL [ARG]…]. For the latter a restart
        completion should be added on all positional arguments.  If
        you allow [TOOL] to be only a restricted set of tools known to
        your program you'd eschew [restart] on the first postional
        argument but add it to the remaining ones.

        {b Warning.} A [restart] directive is eventually emited only
        if the completion is requested after a [--] token. In this
        case other completions returned alongside by {!func} are
        ignored. Educate your users to use the [--], for example
        mention them in {{!page-cookbook.manpage_synopsis}user defined
        synopses}, it is good cli specification hygiene as it properly
        delineates argument scopes. *)

    val message : string -> 'a directive
    (** [message s] is a multi-line, ANSI styled, UTF-8 message reported
        to end users. *)

    val raw : string -> 'a directive
    (** [raw s] takes over the whole {{!page-cli.completion_protocol}protocol}
        output (including subcommand and option name completion) with [s],
        you are in charge. Any other directive in the result of {!func}
        is ignored.

        {b Warning.} The protocol is unstable, it is not advised to
        output it yourself. However this can be useful to invoke
        another tool according to the protocol in the completion
        function and treat its result as the requested completion. *)

    (** {1:completion Completion} *)

    type ('ctx, 'a) func =
      'ctx option -> token:string -> ('a directive list, string) result
    (** The type for completion functions.

        Given an optional context determined from a partial command
        line parse and a token to complete it returns a list of
        completion directives or an error which is reported to
        end-users by using a protocol {!message}.

        The context is [None] if no context was given to {!make} or if
        the context failed to parse on the current command line. *)

    type 'a complete =
    | Complete : 'ctx Term.t option * ('ctx, 'a) func -> 'a complete (** *)
    (** The type for completing.

        A completion context specification which captures a partial
        command line parse (for example the path to a configuration
        file) and a completion function. *)

    type 'a t
    (** The type for completing values parsed into values of type ['a]. *)

    val make : ?context:'ctx Term.t -> ('ctx, 'a) func -> 'a t
    (** [make ~context func] uses [func] to complete.

        [context] defines a commmand line fragment that is evaluated
        before performing the completion. It the evaluation is
        successful the result is given to the completion
        function. Otherwise [None] is given.

        {b Warning.} [context] must be part of the term of the command
        in which you use the completion otherwise the context will
        always be [None] in the function. *)

    val complete : 'a t -> 'a complete
    (** [complete c] completes with [c]. *)

    val complete_files : 'a t
    (** [complete_files] holds a context insensitive function that
        always returns [Ok \[]{!files}[\]]. *)

    val complete_dirs : 'a t
    (** [complete_dirs] holds a context insensitive function that
        always returns [Ok \[]{!dirs}[\]]. *)

    val complete_paths : 'a t
    (** [complete_paths] holds a context insensitive function that
        always returns [Ok \[]{!files}[;]{!dirs}[\]]. *)

    val complete_restart : 'a t
    (** [complete_dirs] holds a context insensitive function that
        always returns [Ok \[]{!restart}[\]]. *)
  end

  (** Argument converters.

      An argument converter transforms a string argument of the command
      line to an OCaml value. {{!converters}Predefined converters}
      are provided for many types of the standard library. *)
  module Conv : sig

    (** {1:converters Converters} *)

    type 'a parser = string -> ('a, string) result
    (** The type for parsing arguments to values of type ['a]. *)

    type 'a fmt = Format.formatter -> 'a -> unit
    (** The type for formatting values of type ['a]. *)

    type 'a t
    (** The type for converting arguments to values of type ['a]. *)

    val make :
      ?completion:'a Completion.t -> docv:string -> parser:'a parser ->
      pp:'a fmt -> unit -> 'a t
    (** [make ~docv ~parser ~pp ()] is an argument converter with
        given properties. See corresponding accessors for semantics. *)

    val of_conv :
      ?completion:'a Completion.t -> ?docv:string ->
      ?parser:'a parser -> ?pp:'a fmt -> 'a t -> 'a t
    (** [of_conv conv ()] is a new converter with given unspecified
        properties defaulting to those of [conv]. *)

    (** {1:properties Properties} *)

    val docv : 'a t -> string
    (** [docv c] is [c]'s documentation meta-variable. This value can
        be refered to as [$(docv)] in the documentation strings of
        arguments.  It can be overriden by the {!val-info} value of an
        argument. *)

    val parser : 'a t -> 'a parser
    (** [parser c] is [c]'s argument parser. *)

    val pp : 'a t -> 'a fmt
    (** [pp c] is [c]'s argument formatter. *)

    val completion : 'a t -> 'a Completion.t
    (** [completion c] is [c]'s completion. *)
  end

  type 'a conv = 'a Conv.t
  (** The type for argument converters. See the
      {{!predef}predefined converters}. *)

  val some' : ?none:'a -> 'a conv -> 'a option conv
  (** [some' ?none c] is like the converter [c] except it returns
      [Some] value. It is used for command line arguments that default
      to [None] when absent. If provided, [none] is used with [c]'s
      formatter to document the value taken on absence; to document
      a more complex behaviour use the [absent] argument of {!val-info}.
      If you cannot construct an ['a] value use {!some}. *)

  val some : ?none:string -> 'a conv -> 'a option conv
  (** [some ?none c] is like [some'] but [none] is described as a
      string that will be rendered in bold. Use the [absent] argument
      of {!val-info} to document more complex behaviours. *)

  (** {1:arginfo Arguments} *)

  type 'a t
  (** The type for arguments holding data of type ['a]. *)

  type info
  (** The type for information about command line arguments.

      Argument information defines the man page information of an
      argument and, for optional arguments, its names. An environment
      variable can also be specified to read get the argument value from
      if the argument is absent from the command line and the variable
      is defined. *)

  val info :
    ?deprecated:string -> ?absent:string -> ?docs:Manpage.section_name ->
    ?doc_envs:Cmd.Env.info list -> ?docv:string -> ?doc:string ->
    ?env:Cmd.Env.info -> string list -> info
  (** [info docs docv doc env names] defines information for
      an argument.
      {ul
      {- [names] defines the names under which an optional argument
         can be referred to. Strings of length [1] like ["c"]) define
         short option names ["-c"], longer strings like ["count"])
         define long option names ["--count"]. [names] must be empty
         for positional arguments.}
      {- [env] defines the name of an environment variable which is
         looked up for defining the argument if it is absent from the
         command line. See {{!page-cli.envlookup}environment variables} for
         details.}
      {- [doc] is the man page information of the argument.
         {{!doc_helpers}These functions} can help with formatting argument
         values.}
      {- [docv] is for positional and non-flag optional arguments.
         It is a variable name used in the man page to stand for their value.
         If unspecified is taken from the argument converter's, see
         {!Conv.docv}.}
      {- [doc_envs] is a list of environment variable that are
         added to the manual of the command when the argument is used.}
      {- [docs] is the title of the man page section in which the argument
         will be listed. For optional arguments this defaults
         to {!Manpage.s_options}. For positional arguments this defaults
         to {!Manpage.s_arguments}. However a positional argument is only
         listed if it has both a [doc] and [docv] specified.}
      {- [deprecated], if specified the argument is deprecated. Use of the
          variable warns on [stderr]. This
          message which should be a capitalized sentence is
          preprended to [doc] and output on standard error when the
          environment variable ends up being used.}
      {- [absent], if specified a documentation string that indicates
         what happens when the argument is absent. The document language
         can be used like in [doc]. This overrides the automatic default
         value rendering that is performed by the combinators.}}

      In [doc], [deprecated], [absent] the
      {{!page-tool_man.doclang}documentation markup language} can be
      used with following variables:

      {ul
      {- ["$(docv)"] the value of [docv] (see below).}
      {- ["$(opt)"], one of the options of [names], preference
        is given to a long one.}
      {- ["$(env)"], the environment var specified by [env] (if any).}} *)

  val ( & ) : ('a -> 'b) -> 'a -> 'b
  (** [f & v] is [f v], a right associative composition operator for
      specifying argument terms. *)

(** {2:optargs Optional arguments}

    The {{!type-info}information} of an optional argument must have at least
    one name or [Invalid_argument] is raised. *)

  val flag : info -> bool t
  (** [flag i] is a [bool] argument defined by an optional flag
      that may appear {e at most} once on the command line under one of
      the names specified by [i]. The argument holds [true] if the
      flag is present on the command line and [false] otherwise. *)

  val flag_all : info -> bool list t
  (** [flag_all] is like {!flag} except the flag may appear more than
      once. The argument holds a list that contains one [true] value per
      occurrence of the flag. It holds the empty list if the flag
      is absent from the command line. *)

  val vflag : 'a -> ('a * info) list -> 'a t
  (** [vflag v \[v]{_0}[,i]{_0}[;…\]] is an ['a] argument defined
      by an optional flag that may appear {e at most} once on
      the command line under one of the names specified in the [i]{_k}
      values. The argument holds [v] if the flag is absent from the
      command line and the value [v]{_k} if the name under which it appears
      is in [i]{_k}.

      {b Note.} Automatic environment variable lookup is unsupported for
      for these arguments but an [env] in an info will be documented.
      Use an option and {!Term.env} for manually looking something up. *)

  val vflag_all : 'a list -> ('a * info) list -> 'a list t
  (** [vflag_all v l] is like {!vflag} except the flag may appear more
      than once. The argument holds the list [v] if the flag is absent
      from the command line. Otherwise it holds a list that contains one
      corresponding value per occurrence of the flag, in the order found on
      the command line.

      {b Note.} Automatic environment variable lookup is unsupported for
      for these arguments but an [env] in an info will be documented.
      Use an option and {!Term.env} for manually looking something up. *)

  val opt : ?vopt:'a -> 'a conv -> 'a -> info -> 'a t
  (** [opt vopt c v i] is an ['a] argument defined by the value of
      an optional argument that may appear {e at most} once on the command
      line under one of the names specified by [i]. The argument holds
      [v] if the option is absent from the command line. Otherwise
      it has the value of the option as converted by [c].

      If [vopt] is provided the value of the optional argument is
      itself optional, taking the value [vopt] if unspecified on the
      command line.  {b Warning} using [vopt] is
      {{!page-cookbook.tip_avoid_default_option_values}not
      recommended}. *)

  val opt_all : ?vopt:'a -> 'a conv -> 'a list -> info -> 'a list t
  (** [opt_all vopt c v i] is like {!opt} except the optional argument may
      appear more than once. The argument holds a list that contains one value
      per occurrence of the flag in the order found on the command line.
      It holds the list [v] if the flag is absent from the command line. *)

  (** {2:posargs Positional arguments}

      The {{!type-info}information} of a positional argument must have no name
      or [Invalid_argument] is raised. Positional arguments indexing
      is zero-based.

      {b Warning.} The following combinators allow to specify and
      extract a given positional argument with more than one term.
      This should not be done as it will likely confuse end users and
      documentation generation. These over-specifications may be
      prevented by raising [Invalid_argument] in the future. But for now
      it is the client's duty to make sure this doesn't happen. *)

  val pos : ?rev:bool -> int -> 'a conv -> 'a -> info -> 'a t
  (** [pos rev n c v i] is an ['a] argument defined by the [n]th
      positional argument of the command line as converted by [c].
      If the positional argument is absent from the command line
      the argument is [v].

      If [rev] is [true] (defaults to [false]), the computed
      position is [max-n] where [max] is the position of
      the last positional argument present on the command line. *)

  val pos_all : 'a conv -> 'a list -> info -> 'a list t
  (** [pos_all c v i] is an ['a list] argument that holds
      all the positional arguments of the command line as converted
      by [c] or [v] if there are none. *)

  val pos_left :
    ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t
  (** [pos_left rev n c v i] is an ['a list] argument that holds
      all the positional arguments as converted by [c] found on the left
      of the [n]th positional argument or [v] if there are none.

      If [rev] is [true] (defaults to [false]), the computed
      position is [max-n] where [max] is the position of
      the last positional argument present on the command line. *)

  val pos_right :
    ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t
  (** [pos_right] is like {!pos_left} except it holds all the positional
      arguments found on the right of the specified positional argument. *)

  (** {2:argterms Converting to terms} *)

  val value : 'a t -> 'a Term.t
  (** [value a] is a term that evaluates to [a]'s value. *)

  val required : 'a option t -> 'a Term.t
  (** [required a] is a term that fails if [a]'s value is [None] and
      evaluates to the value of [Some] otherwise. Use this in combination
      with {!Arg.some'} for required
      positional arguments. {b Warning} using this on optional arguments
      is {{!page-cookbook.tip_avoid_required_opt}not recommended}. *)

  val non_empty : 'a list t -> 'a list Term.t
  (** [non_empty a] is term that fails if [a]'s list is empty and
      evaluates to [a]'s list otherwise. Use this for non empty lists
      of positional arguments. *)

  val last : 'a list t -> 'a Term.t
  (** [last a] is a term that fails if [a]'s list is empty and evaluates
      to the value of the last element of the list otherwise. Use this
      for lists of flags or options where the last occurrence takes precedence
      over the others. *)

  (** {2:predef Predefined arguments} *)

  val man_format : Manpage.format Term.t
  (** [man_format] is a term that defines a [--man-format] option and
      evaluates to a value that can be used with {!Manpage.print}. *)

  (** {1:converters Predefined converters} *)

  val bool : bool conv
  (** [bool] converts values with {!bool_of_string}. *)

  val char : char conv
  (** [char] converts values by ensuring the argument has a single char. *)

  val int : int conv
  (** [int] converts values with {!int_of_string}. *)

  val nativeint : nativeint conv
  (** [nativeint] converts values with {!Nativeint.of_string}. *)

  val int32 : int32 conv
  (** [int32] converts values with {!Int32.of_string}. *)

  val int64 : int64 conv
  (** [int64] converts values with {!Int64.of_string}. *)

  val float : float conv
  (** [float] converts values with {!float_of_string}. *)

  val string : string conv
  (** [string] converts values with the identity function. *)

  val enum : ?docv:string -> (string * 'a) list -> 'a conv
  (** [enum l p] converts values such that string names in [l] map to
      the corresponding value of type ['a]. [docv] is the converter's
      documentation meta-variable, it defaults to [ENUM].  A
      {{!Completion.make}completion} is added for the names.

      {b Warning.} The type ['a] must be comparable with {!Stdlib.compare}.

      @raise Invalid_argument if [l] is empty. *)

  val list : ?sep:char -> 'a conv -> 'a list conv
  (** [list sep c] splits the argument at each [sep] (defaults to [','])
      character and converts each substrings with [c]. *)

  val array : ?sep:char -> 'a conv -> 'a array conv
  (** [array sep c] splits the argument at each [sep] (defaults to [','])
      character and converts each substring with [c]. *)

  val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
  (** [pair sep c0 c1] splits the argument at the {e first} [sep] character
      (defaults to [',']) and respectively converts the substrings with
      [c0] and [c1]. *)

  val t2 : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
  (** {!t2} is {!pair}. *)

  val t3 : ?sep:char -> 'a conv ->'b conv -> 'c conv -> ('a * 'b * 'c) conv
  (** [t3 sep c0 c1 c2] splits the argument at the {e first} two [sep]
      characters (defaults to [',']) and respectively converts the
      substrings with [c0], [c1] and [c2]. *)

  val t4 :
    ?sep:char -> 'a conv -> 'b conv -> 'c conv -> 'd conv ->
    ('a * 'b * 'c * 'd) conv
  (** [t4 sep c0 c1 c2 c3] splits the argument at the {e first} three [sep]
      characters (defaults to [',']) respectively converts the substrings
      with [c0], [c1], [c2] and [c3]. *)

  (** {2:files Files and directories} *)

  val path : string conv
  (** [path] is like {!string} but prints using {!Filename.quote}
      and completes both files and directories. *)

  val filepath : string conv
  (** [filepath] is like {!string} but prints using {!Filename.quote}
      and completes files. *)

  val dirpath : string conv
  (** [dirpath] is like {!string} but prints using {!Filename.quote}
      and completes directories. *)

  (** {b Note.} The following converters report errors whenever the
      requested file system object does not exist. This is only mildly
      useful since nothing guarantees they will still exist at the
      time you act upon them. So you will have to treat these error
      cases anyways in your tool function. It is also unhelpful if the file
      system object may be created by your tool. Rather use
      {!filepath} and {!dirpath}. *)

  val file : string conv
  (** [file] converts a value with the identity function and checks
      with {!Sys.file_exists} that a file with that name exists.  The
      string ["-"] is parsed without checking: it represents [stdio].
      It completes both files directories. *)

  val dir : string conv
  (** [dir] converts a value with the identity function and checks
      with {!Sys.file_exists} and {!Sys.is_directory} that a directory
      with that name exists. It completes directories. *)

  val non_dir_file : string conv
  (** [non_dir_file] converts a value with the identity function and
      checks with {!Sys.file_exists} and {!Sys.is_directory} that a
      non directory file with that name exists. The string ["-"] is
      parsed without checking it represents [stdio].  It completes
      files. *)

  (** {1:doc_helpers Documentation formatting helpers} *)

  val doc_quote : string -> string
  (** [doc_quote s] quotes the string [s]. *)

  val doc_alts : ?quoted:bool -> string list -> string
  (** [doc_alts alts] documents the alternative tokens [alts]
      according the number of alternatives. If [quoted] is:
      {ul
      {- [None], the tokens are enclosed in manpage markup directives
         to render them in bold (manpage convention).}
      {- [Some true], the tokens are quoted with {!doc_quote}.}
      {- [Some false], the tokens are written as is}}
      The resulting string can be used in sentences of
      the form ["$(docv) must be %s"].

      @raise Invalid_argument if [alts] is the empty list.  *)

  val doc_alts_enum : ?quoted:bool -> (string * 'a) list -> string
  (** [doc_alts_enum quoted alts] is [doc_alts quoted (List.map fst alts)]. *)

  (** {1:deprecated Deprecated}

      These identifiers are silently deprecated. For now there is no
      plan to remove them. But you should prefer to use the {!Conv}
      interface in new code. *)

  type 'a printer = 'a Conv.fmt
  (** Deprecated. Use {!Conv.fmt}. *)

  val conv' : ?docv:string -> 'a Conv.parser * 'a Conv.fmt -> 'a conv
  (** Deprecated. Use {!Conv.make} instead. *)

  val conv :
    ?docv:string -> (string -> ('a, [`Msg of string]) result) * 'a Conv.fmt ->
    'a conv
  (** Deprecated. Use {!Conv.make} instead. *)

  val conv_parser : 'a conv -> (string -> ('a, [`Msg of string]) result)
  (** Deprecated. Use {!Conv.val-parser}. *)

  val conv_printer : 'a conv -> 'a Conv.fmt
  (** Deprecated. Use {!Conv.val-pp}. *)

  val conv_docv : 'a conv -> string
  (** Deprecated. Use {!Conv.val-docv}. *)

  val parser_of_kind_of_string :
    kind:string -> (string -> 'a option) ->
    (string -> ('a, [`Msg of string]) result)
  (** Deprecated. [parser_of_kind_of_string ~kind kind_of_string] is an argument
      parser using the [kind_of_string] function for parsing and [kind]
      to report errors (e.g. could be ["an integer"] for an [int] parser.). *)
end
