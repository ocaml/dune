%token EOF

%start <Ast.expr> expr

%%

expr:
| EOF { Ast.Unit }
