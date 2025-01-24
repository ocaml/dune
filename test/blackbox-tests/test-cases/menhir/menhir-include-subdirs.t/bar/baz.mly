%token EOF

%start<unit> unit
%%

unit:
| EOF { Unit.unit }
