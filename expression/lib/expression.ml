include Ast

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof Lexer.token lexbuf in
     ast

let string_of_declaration = String_of.string_of_declaration

let rec mkConstructorApp expr variantvars = match variantvars with
  | [] -> expr
  | [var, _] -> Application (expr, Identifier var)
  | (var, _)::rest ->
    Application (expr, mkConstructorApp (Application (Identifier ",", Identifier var)) rest)

let caseproof _vars varnm _typenm rules lhs rhs (variantnm, _variantvars) =
let variant_namedvars = [("?1","int");("?2","list")] (* TODO: construct this from variantvars *) in
   let variant_expr = mkConstructorApp (Identifier variantnm) variant_namedvars in
   let caserule = ([], "case", Identifier varnm, variant_expr) in
let ihs = [] (* TODO: generate the inductive hypotheses for all variant_namedvars that are of type typenm *) in
   let variantrules = caserule::ihs @ rules in
   ("Case "^ variantnm ^ ":") ::
   List.map (fun (vars, nm, lhs, rhs) -> nm^": "^String.concat "" (List.map (fun x -> "Forall "^x^". ") vars)^String_of.string_of_expression lhs^" = "^String_of.string_of_expression rhs) ihs @
   Prove.prove variantrules lhs rhs @ ["This completes the proof of case "^ variantnm ^ ".";""]

let inductionproof varnm _types vars rules lhs rhs =
   let typenm = "list" (* TODO: don't hard-code the type! *) in
   let variants = [("Cons",["int";"list"]) ; ("Nil" , [])] (* TODO: find the type in 'types' and convert it to this! *) in
   ("Proof by induction on (" ^ varnm ^ " : "^ typenm^") :") ::
   List.(concat (map (caseproof vars varnm typenm rules lhs rhs) variants)) @
   ["This completes the proof by induction."]

let rec prover types rules declarations =
      match declarations with
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
            -> (* no hint, so let's prove this *)
               Prove.prove rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some Axiom) :: rest
            -> (* we got an axiom hint so we simply assume the statement *)
               prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some (Induction varnm)) :: rest
            -> (* we got an induction hint so we do that for the statement *)
               inductionproof varnm types vars rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | TypeDeclaration (nm, variants) :: rest
            -> (* add the type to the list of types and keep going *)
            prover ((nm, variants)::types) rules rest
         | FunctionDeclaration _ :: rest -> prover types rules rest
         | [] -> []


let prover_main decls =
      prover [] [] decls |>
      List.map (String.concat "\n") |>
      String.concat "\n\n" |>
      print_endline

