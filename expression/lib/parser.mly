%{
    open Ast
%}

%token <string> IDENT
%token <string> HINT
%token LET
%token PROVE
%token EOF
%token EQ
%token LPAREN
%token RPAREN
%token COLON
%token PIPE
%token ARROW
%token COMMA
%token TYPE
%token MATCH
%token WITH
%token CASE
%token OF
%token REC
%start main
%type <declaration list> main
%%
main:
 |e = list(declaration); EOF { e } 
;

// statement:
// | decl = declaration; { Declaration(decl) }
// | eq = equality; { Equality(eq) }
// | exp = expression; { Expression(exp) }
// ;

expression:
| LPAREN; e = expression; RPAREN { e }
| nm = IDENT { Identifier(nm) }
| e1 = expression; nm = IDENT { Application(e1, Identifier(nm)) }
| e1 = expression; LPAREN; e2 = expression; RPAREN { Application(e1, e2) }
| e1 = expression; EQ; e2 = expression; { Equality(e1, e2) }
| MATCH; e1 = expression; WITH; c = cases; { Match(e1, c )}
| e1 = expression COMMA e2 = expression { Tuple(e1, e2) };

// tuple:
// | LPAREN e1 = expression COMMA e2 = expression RPAREN { [e1, e2] }
// | LPAREN e = expression COMMA els = tuple RPAREN { [e::els] };


declaration:
| LET; id = IDENT; arguments = arguments; EQ; e = expression { 
    Let(id, arguments, e) 
  }
| LET; PROVE; id = IDENT; EQ; e = expression { 
    Let(id, [], e) 
  }
| LET; PROVE; id = IDENT;  arguments = arguments; EQ; e = expression { 
    LetProve(id, arguments, e) 
  }
| LET; REC; id = IDENT; arguments = arguments; EQ; e = expression { 
    LetRec(id, arguments, e) 
  }
| h = HINT { Hint(h) };

cases: 
| PIPE e1 = expression ARROW e2 = expression { [(e1, e2)] }
| PIPE e1 = expression ARROW e2 = expression cs = cases { (e1, e2) :: cs };

// variants: 
// | PIPE e1 = expression ARROW e2 = expression { [(e1, e2)] }
// | PIPE e1 = expression ARROW e2 = expression cs = cases { (e1, e2) :: cs };

arguments:
| LPAREN id = IDENT COLON typ = IDENT RPAREN { [(id, typ)] }
| LPAREN id = IDENT COLON typ = IDENT RPAREN args = arguments { (id, typ) :: args };

