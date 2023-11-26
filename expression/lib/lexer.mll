{
    open Parser
    exception SyntaxError of string
    let buffer = Buffer.create 128   (* chat gpt helped figure this line out *)
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | "(*prove*)" { PROVE }
 (* | "(*hint:" { Buffer.clear buffer; hint lexbuf } *)
 | "(*hint:" { HINT }
 | "axiom" { AXIOM }
 (* | "(*" { comment 1 lexbuf } *)
 | "(*" { comment 0 lexbuf }
 | "*)" { ENDCOMMENT }
 | "let" { LET }
 | "rec" { REC }
 | "of" { OF }
 | "type" { TYPE }
 | "match" { MATCH }
 | "with" { WITH }
 | "=" { EQ }
 | "," {COMMA}
 | ":" { COLON }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | '|' { PIPE }
 | "->" { ARROW }
 | ['a'-'z' 'A'-'Z' '0'-'9' '?' '_' '-' '\'']+ as id { IDENT id }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and comment level = parse
 | "*)" { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
 | "(*" { comment (level + 1) lexbuf }
 | newline { Lexing.new_line lexbuf; comment level lexbuf }
 | _ { comment level lexbuf }
 | eof { raise (SyntaxError "Unclosed comment") }

(* and hint = parse
 | "*)" { HINT (Buffer.contents buffer) }
 | newline { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; hint lexbuf }
 | _ as char { Buffer.add_char buffer char; hint lexbuf } *)
   (* ^ chat gpt helped figure this part out with the buffer ^ *)
    


 