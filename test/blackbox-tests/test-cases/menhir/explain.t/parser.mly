%token START
%start <int> start
%%
start: START { 42 }
