<refentry>

<refmeta>
  <refentrytitle>META</refentrytitle>
  <manvolnum>5</manvolnum>
  <refmiscinfo>The findlib package manager for OCaml</refmiscinfo>
</refmeta>

<refnamediv id="META">
  <refname>META</refname>
  <refpurpose>[File that specifies metainformation of OCaml packages]</refpurpose>
</refnamediv>


<refsynopsisdiv>
<title>GRAMMAR</title>
<synopsis>
            entry ::= variable_name [ formal_predicates ] '=' value
formal_predicates ::= '(' formal_predicate { ',' formal_predicate } ')'
    variable_name ::= name
 formal_predicate ::= name
             name ::= [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ]+
            value ::= '"' character* '"'
</synopsis>
</refsynopsisdiv>

<refsect1>
<title>DESCRIPTION</title>
<para>
If a package directory contains a file with the fixed name "META" it
is interpreted as described here. The file is a sequence of entries
following the given grammar; every entry defines a variable under a
certain condition given by the list of formal predicates.
</para>

<para>
There is a list of predefined variables and a list of standard
predicates. These variables define: required packages, version
information, directories, archive files, and linker options. The
predicates denote circumstances of the application of the variables:
whether the bytecode or the native compiler is used, if there is a
toploop compiled in, details of multi-threading execution, details of
profiling. 
</para>
</refsect1>

<refsect1>
<title>DETAILS OF THE FILE FORMAT</title>
<para>
The file consists of a sequence of entries which must be formed as the
grammar prescribes. The lexical tokens are names, values, and
interpunctuation like '(', ',' and so on. Note that linefeeds do not
play a special role, i.e. an entry definition may be given in more than
one line, or several definitions may occur on a single line. There may
be comments which begin with '#' and consist of the rest of the line.
</para>

<para>
Names are sequences of the characters A-Z, a-z, 0-9, or _. Names
containing capital letters and names beginning with digits are
allowed but not recommended.
</para>

<para>
Values are enclosed between double quotes. Values may contain any
character. The characters " and \ must be preceded by backslashes. 
</para>
</refsect1>

<refsect1>
<title>SEMANTICS OF DEFINITIONS</title>
<para>
There may be multiple definitions for the same variable if the lists of
formal predicates are different. The effective definition is selected
upon a set of actual predicates, i.e. the set of predicates that are
assumed to be true. In order to determine the value of a variable, the
following rule applies:
</para>

<itemizedlist mark="bullet" spacing="compact"> 
<listitem> 
<para> 
A definition can only be used if all formal parameters are included in
the set of actual parameters, such a definition is called
<emphasis>applicable</emphasis>. If there is no such definition, the
variable has no value.
</para>
</listitem>

<listitem>
<para>
If there is more than one applicable definition, the definition with
the biggest number of formal parameters is selected.
</para>
</listitem>

<listitem>
<para>
If there is still more than one definition, both applicable and with a
maximum number of formal parameters, the definition that is defined
first is selected.
</para>
</listitem>
</itemizedlist>
</refsect1>

<refsect1>
<title>VARIABLES</title>

<para>
There is a set of variables with predefined meaning:
</para>

<itemizedlist mark="bullet" spacing="compact"> 
<listitem> 
<para> 
The variable "directory" redefines the location of the package
directory. Normally, the META file is the first file read in the
package directory, and before any other file is read, the "directory"
variable is evaluated in order to see if the package directory must be
changed. The value of the "directory" variable is determined with an
empty set of actual predicates. The value must be either: an absolute
path name of the alternate directory, or a path name relative to the
stdlib directory of OCaml (written "^path" or "+path").
</para>
</listitem>

<listitem> 
<para> 
The variable "requires" specifies the list of required packages. The
names of the packages must be separated by white space and/or commas.
</para>
</listitem>

<listitem> 
<para> 
The variable "version" specifies the version string.
</para>
</listitem>

<listitem> 
<para> 
The variable "archive" specifies the list of archive files. These
files should be given as plain names without any directory
information; they are only searched in the package directory. The
names of the files must be separated by white space and/or commas.
In the preprocessor stage, the archive files are passed as extensions
to the preprocessor (camlp4) call. In the linker stage (-linkpkg), the archive
files are linked. In the compiler stage, the archive files are ignored.
</para>
</listitem>

<listitem> 
<para> 
The variable "linkopts" specifies additional linker options.
</para>
</listitem>

<listitem> 
<para> 
The variable "description" may include a short description of the
package (displayed by <literal>ocamlfind list</literal>).
</para>
</listitem>


</itemizedlist>

<para>
It is possible to define additional variables but there is currently
no software interpreting them.
</para>
</refsect1>


<refsect1>
<title>PREDICATES</title>

<para>
There is a list of standard predicates:
</para>

<itemizedlist mark="bullet" spacing="compact">
<listitem>
<para>
The "byte" predicate means that the bytecode compiler is used.
</para>
</listitem>

<listitem>
<para>
The "native" predicate means that the native compiler is used.
</para>
</listitem>

<listitem>
<para>
The "toploop" predicate means that the toploop is available in the
linked program.
</para>
</listitem>

<listitem>
<para>
The "mt" predicate means that the program is multi-threaded.
</para>
</listitem>

<listitem>
<para>
The "mt_posix" predicate means that in the case "mt" is set, too, the
POSIX libraries are used to implement threads.
</para>
</listitem>

<listitem>
<para>
The "mt_vm" predicate means that in the case "mt" is set, too, the
VM-based libraries are used to implement threads.
</para>
</listitem>

<listitem>
<para>
The "gprof" predicate means that in the case "native" is set, too, the
program is compiled for profiling
</para>
</listitem>

<listitem>
<para>
The "autolink" predicate means that ocamlc can/will perform automatic linking.
</para>
</listitem>

<listitem>
<para>
The "preprocessor" predicate means that the META variables are scanned for
preprocessor options.</para>
</listitem>

<listitem>
<para>
The "syntax" predicate means that the -syntax option is present on the
command line.</para>
</listitem>

</itemizedlist>
</refsect1>

</refentry>
