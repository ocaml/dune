We have 3 opam packages depending on each other in order. We test how setenv interacts
with the various env update operators.

  $ . ./env-update-helpers.sh

  $ test 
  package1
    [ override = "package1" ]
    [ prepend_sep := "package1" ]
    [ prepend += "package1" ]
    [ append_sep =: "package1" ]
    [ append =+ "package1" ]
  package2
    [ override = "package2" ]
    [ prepend_sep := "package2" ]
    [ prepend += "package2" ]
    [ append_sep =: "package2" ]
    [ append =+ "package2" ]
  package3
    [ override = "package3" ]
    [ prepend_sep := "package3" ]
    [ prepend += "package3" ]
    [ append_sep =: "package3" ]
    [ append =+ "package3" ]
  
  package1:
     override: initial value
  prepend_sep: initial value
      prepend: initial value
   append_sep: initial value
       append: initial value
  
  package2:
     override: initial value
  prepend_sep: initial value
      prepend: initial value
   append_sep: initial value
       append: initial value
  
  package3:
     override: initial value
  prepend_sep: initial value
      prepend: initial value
   append_sep: initial value
       append: initial value

Here are some properties the values above should have:

= override
The following should hold:
- package1 should have the initial value
- package(n) shuld have value package(n-1)
This does not currently hold.
 
:= prepend seperator
The following should hold:
- All values should have a trailing seperator :
- package1 should have the initial value
- package3 should output "package2:package1:initial value:"
This does not currently hold.

+= prepend
The following should hold:
- package1 should have the initial value
- package3 should output "package2:package1:initial value"
This does not currently hold.

=: append seperator
The following should hold:
- All values should have a leading seperator :
- package1 should have the initial value
- package3 should output ":initial value:package1:package2"
This does not currently hold.

=+ append
The following should hold:
- package1 should have the initial value
- package3 should output "initial value:package1:package2"
This does not currently hold.
