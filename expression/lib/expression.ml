include Ast
(* include Substitution *)

module Parser = Parser
module Lexer = Lexer


(* module type Match = sig
(* vars -> pattern -> goal -> possible substitution *)
  val match_expressions : string list -> expression -> expression -> Substitution.t option
end *)

(* Given a list of variables, an equality to apply, and an expression to
   apply it to, returns the result to applying the equality to the expression
   (if possible) *)
let attemptRewrite (lst : string list) (axiom : equality) (exp : expression) : expression option =
  (* match match_expressions variables lhs expr with
  | ... (* matches! *) -> Some (...)
  | ... (* not a match *) -> (match expr with
      | App (fn, arg) -> (match attemptRewrite ... arg with
          | Some v -> Some (App (fn, v))
          ...) *)
  failwith "yo frick"

(* Given an expression, tries to apply each equality in the list. Returns the
   name of the rule that was successfully applied and the resulting
   expression 
      returns the pair (eqn, expr') where eqn is the name of the equation that
          applied and expr' is the rewritten expression
      Simply calls the attemptRewrite function for each of the equalities *)
let rec tryEqualities (exp: expression) (steps: (string * string list * equality) list) : (string * expression) option =
  match steps with
  | (name, vars, eq) :: tl -> (match attemptRewrite vars eq exp with
      | Some expr -> Some (name, expr)
      | None -> tryEqualities exp tl)
  | [] -> None

let rec performSteps (exp: expression) (steps: (string * string list * equality) list) : (string * expression) list =
  match tryEqualities exp steps with
  | Some (s, exp2) -> (s, exp2) :: performSteps exp2 steps
  | None -> []

let produceProof (eq: equality) (steps: (string * string list * equality) list) : string list =
  let Equality(lhs, rhs) = eq in
  let lhsoln = performSteps lhs steps in
  let rhsoln = performSteps rhs steps in
  (* i lack critical information on the fundamental way to check if two
     things have matching shape *)
  failwith "not implemented"

(* print out each declaration with one of the things below
   and the string substitution information which comes from somewhere *)
let produce_output_simple (steps: declaration list) : string =
  failwith "not implemented"


let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast


let rec string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  | Variable (name, type_name) -> name ^ " : " ^ type_name

let rec string_of_expression (e : expression) : string =
  match e with
  (* If you're confused about the structure of the AST,
     you can use this code to print more parentheses
     (besides using utop):
  | Application (Application (Identifier ",", e), arg) ->
    (string_of_expression_paren e) ^ ", " ^ (string_of_expression_paren arg)
  | Application (e, arg) ->
    (string_of_expression_paren e) ^ " " ^ string_of_expression_paren arg
     
     *)
  | Application (Application (Identifier ",", e), arg) ->
    (string_of_expression e) ^ ", " ^ (string_of_expression arg)
  | Application (e, arg) ->
    (string_of_expression e) ^ " " ^ string_of_expression_paren arg
  | Identifier name -> name
  | Match (e, cases) ->
    let case_strings = List.map (fun (pattern, body) ->
      let pattern_string = match pattern with
        | Constructor (name, []) -> name
        | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
        | Variable (name, type_name) -> name ^ " : " ^ type_name
      in
      (* the outer parentheses are redundant if the body does not end in a match, but better to be safe then sorry *)
      pattern_string ^ " -> " ^ (string_of_expression_paren body)
    ) cases in
    "match " ^ (string_of_expression e) ^ " with " ^ (String.concat " | " case_strings)

and string_of_expression_paren (e : expression) : string =
  match e with
  | Identifier name -> name
  | e -> "(" ^ string_of_expression e ^ ")"

let string_of_hint (h : hint option) : string =
  match h with
  | Some Axiom -> "\n(*hint: axiom *)"
  | Some (Induction name) -> "\n(*hint: induction " ^ name ^ " *)"
  | None -> ""
let string_of_equality (e : equality) : string =
  match e with
  | Equality (e1, e2) -> "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"
let string_of_typedvariable (TypedVariable (name, type_name) : typedVariable) : string =
  "(" ^ name ^ " : " ^ type_name ^ ")"
let string_of_declaration (d : declaration) : string =
  match d with
  | TypeDeclaration (name, variants) ->
    let variant_strings = List.map (function Variant (name, []) -> name
      | Variant (name, types) -> name ^ " of (" ^ (String.concat "*" types) ^ ")"
    ) variants in
    "type " ^ name ^ " = " ^ (String.concat " | " variant_strings)
  | FunctionDeclaration (TypedVariable (name, type_name), args, body) ->
    let arg_strings = List.map (function TypedVariable (name, type_name) -> "(" ^ name ^ " : " ^ type_name ^ ")") args in
    "let rec " ^ name ^ " " ^ (String.concat " " arg_strings) ^ " : " ^ type_name ^ " = " ^ (string_of_expression body)
  | ProofDeclaration (name, args, equality, hint) ->
    let arg_strings = List.map string_of_typedvariable args in
    "let (*prove*) " ^ name ^ " " ^ (String.concat " " arg_strings) ^ " = "
     ^ string_of_equality equality ^ string_of_hint hint
