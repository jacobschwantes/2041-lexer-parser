%{
    open Ast
%}

%token <string> IDENT
// %token <string> HINT
%token HINT
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
%token ENDCOMMENT /* there is no startcomment, as it's called "hint", and proper comments are ignored by the lexer */
%token TYPE
%token MATCH
%token WITH
%token CASE
%token OF
%token REC
%token AXIOM
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

equality:
| LPAREN ; e = equality ; RPAREN { e }
| lhs = expression ; EQ ; rhs = expression { Equality (lhs, rhs) }
hint:
| HINT ; AXIOM ; ENDCOMMENT { Axiom }
expression:
| LPAREN; e = expression_with_commas; RPAREN { e }
| nm = IDENT { Identifier(nm) }
| e1 = expression; nm = IDENT { Application(e1, Identifier(nm)) }
| e1 = expression; LPAREN; e2 = expression; RPAREN { Application(e1, e2) }
// | e1 = expression; EQ; e2 = expression; { Equality(e1, e2) }
| MATCH; e1 = expression; WITH; c = cases; { Match(e1, c )}
| e1 = expression COMMA e2 = expression { Tuple(e1, e2) };

// these aren't in the gettingstarted.ml syntax,
// but here's a suggestion to deal with these anyways.
// We're using that "," is not a valid identifier
// We're using it as an identifier that stands for the function (fun x y -> (x, y))
// This also means we're representing (x,y,z) and ((x,y),z) as the same thing.
expression_with_commas:
| e = expression { e }
| e1 = expression_with_commas ; COMMA ; e2 = expression
  { Application (Application (Identifier ",", e1), e2)}

// tuple:
// | LPAREN e1 = expression COMMA e2 = expression RPAREN { [e1, e2] }
// | LPAREN e = expression COMMA els = tuple RPAREN { [e::els] };


declaration:
| LET ; PROVE ; lemma_name=IDENT ; args = list(argument) ; EQ ; eq = equality ; hint=option(hint)
    { Declaration(lemma_name, args, eq, hint) }
// | LET; id = IDENT; arguments = arguments; EQ; e = expression { 
//     Let(id, arguments, e) 
//   }
// | LET; PROVE; id = IDENT; EQ; e = expression { 
//     Let(id, [], e) 
//   }
// | LET; PROVE; id = IDENT;  arguments = arguments; EQ; e = expression { 
//     LetProve(id, arguments, e) 
//   }
// | LET; REC; id = IDENT; arguments = arguments; EQ; e = expression { 
//     LetRec(id, arguments, e) 
//   }
// | h = HINT { Hint(h) };

cases: 
| PIPE e1 = expression ARROW e2 = expression { [(e1, e2)] }
| PIPE e1 = expression ARROW e2 = expression cs = cases { (e1, e2) :: cs };

// variants: 
// | PIPE e1 = expression ARROW e2 = expression { [(e1, e2)] }
// | PIPE e1 = expression ARROW e2 = expression cs = cases { (e1, e2) :: cs };

//uhhhhhhhhhhhhhhhhh
argument:
| nm = IDENT; COLON; t = IDENT { TypedVariable (nm, t) }
| LPAREN ; arg = argument; RPAREN { arg }

arguments:
| LPAREN id = IDENT COLON typ = IDENT RPAREN { [(id, typ)] }
| LPAREN id = IDENT COLON typ = IDENT RPAREN args = arguments { (id, typ) :: args };

