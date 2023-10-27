%token TOKEN
%token EOF

%start <unit> main

%%

main:
| TOKEN TOKEN EOF { () }
| list(TOKEN) EOF { () }
