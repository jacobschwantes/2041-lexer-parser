{
    open Parser
    exception SyntaxError of string
    let buffer = Buffer.create 128
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | "(*prove*)" { PROVE }
 | "(*hint" { Buffer.clear buffer; hint lexbuf }
 | "(*" { comment 1 lexbuf }
 | "let" { LET }
 | ['a'-'z' 'A'-'Z' '0'-'9' '?' '_' '-' '\'']+ as id { IDENT id }
 | "=" { EQ }
 | ":" { COLON }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and comment level = parse
 | "*)" { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
 | "(*" { comment (level + 1) lexbuf }
 | newline { Lexing.new_line lexbuf; comment level lexbuf }
 | _ { comment level lexbuf }
and hint = parse
 | "*)" { HINT (Buffer.contents buffer) }
 | newline { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; hint lexbuf }
 | _ as char { Buffer.add_char buffer char; hint lexbuf }
    


 