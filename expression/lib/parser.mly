%{
    open Ast
%}

%token <string> IDENT
// %token <string> HINT
%token LET
%token PROVE
%token EOF
%token EQ
%token LPAREN
%token RPAREN
%token COLON
%start main
%type <expression list> main
%%
main:
 |e = list(expression); EOF { e } 
;

expression:
| LET; PROVE; id = IDENT;  arguments = arguments; EQ; e = expression { 
    Let(id, arguments, e) 
  }
| e1 = expression; EQ; e2 = expression { Equality(e1, e2) }
| LPAREN; e = expression; RPAREN { e }
| nm = IDENT { Identifier(nm) }
| e1 = expression; nm = IDENT { Application(e1, Identifier(nm)) }
| e1 = expression; LPAREN; e2 = expression; RPAREN { Application(e1, e2) };


arguments:
| LPAREN id = IDENT COLON typ = IDENT RPAREN { [(id, typ)] }
| LPAREN id = IDENT COLON typ = IDENT RPAREN args = arguments { (id, typ) :: args };


