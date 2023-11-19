include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_expression (e: expression) = match e with
  | Identifier s -> s
  | Application (e1, e2) ->
      (string_of_expression e1) ^ " (" ^ (string_of_expression e2) ^ ")"
  | Equality (e1, e2) ->
      "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"
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




