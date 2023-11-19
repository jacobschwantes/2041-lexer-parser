{
    open Parser
    exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
  | "(*prove*)" { token lexbuf }
| "let" { LET }
 | "(*" { comment lexbuf }
 | "=" { EQ }
 | ":" { COLON }
 | ['a'-'z' 'A'-'Z' '0'-'9' '?' '_' '\'']+ as id
{ IDENT id }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and comment = parse
| "*)" { token lexbuf }
| newline { Lexing.new_line lexbuf;token lexbuf }
| _ { comment lexbuf }

 