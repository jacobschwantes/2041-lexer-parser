include Ast
module Parser = Parser
module Lexer = Lexer



let rec string_of_expression (e: expression) = match e with
| Identifier s -> s
| Application (e1, e2) ->
  (string_of_expression e1) ^ " (" ^ (string_of_expression e2) ^ ")"
| Equality (e1, e2) ->
    "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"
| Match (e1, c) ->
    "match " ^ (string_of_expression e1) ^ " with " ^ (string_of_cases c)
| Tuple (e1, e2) ->
    "(" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e2) ^ ")"
and string_of_cases e = match e with 
| [] -> ""
| (c1, c2)::tl -> "|" ^ (string_of_expression c1) ^ " -> " ^ (string_of_expression c2) ^ (string_of_cases tl)
and string_of_declaration e = match e with
| Hint content -> 
  "(*hint:" ^ content ^ "*)"
| Let (id, typed_args, expr) ->
  "let " ^ id ^ " " ^ (string_of_arguments typed_args) ^ " = " ^ (string_of_expression expr)
| LetProve (id, typed_args, expr) ->
  "let (*prove*) " ^ id ^ " " ^ (string_of_arguments typed_args) ^ " = " ^ (string_of_expression expr)
| LetRec (id, typed_args, expr) ->
  "let rec " ^ id ^ " " ^ (string_of_arguments typed_args) ^ " = " ^ (string_of_expression expr)
  and string_of_arguments args =
    String.concat " " (List.map string_of_argument args)
  and string_of_argument (name, typ) =
    "(" ^ name ^ " : " ^ typ ^ ")"






